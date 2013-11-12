
(define run-next-on-queue
  (lambda ()
    (letrec 
	([checker
	  (lambda ()
	    (let ([front (get 'fptr)]
		  [back  (get 'bptr)]
		  [timer (get 'tptr)])
	      ;;If the timer queue isn't empty, we ain't deadlocked.
	      (cond
	       [(and (equal? front back)
		     (equal? front 'NotProcess.p)
		     (equal? timer 'NotTimerProcess.p))
		(error "Deadlock in attempting to run next on queue")]
	       
	       ;;If there's something on the timer queue, and it's 
	       ;; ready to run, we should run it. 
	       [(and (not (equal? timer 'NotTimerProcess.p))
		     (AFTER? (current-milliseconds) (get 'tnext)))
		
		
		(set 'iptr (workspace timer iptr))
		(set 'wptr timer)
		(set 'tptr  (workspace timer next-t))
		
		(if (not (equal? (get 'tptr) 'NotTimerProcess.p))
		    (set 'tnext (workspace (get 'tptr) timeout)))
		]
	       
	       ;; We're not deadlocked, there's nothing on the timer 
	       ;; queue, so it seems like we're free to run things
	       ;; off the regular queue. 

	       [(not (equal? front 'NotProcess.p))

		(set 'wptr front)
		(set 'iptr (workspace wptr iptr))
		
		(cond
		 [(equal? front back)
		  ;; last on queue.
		  (set 'fptr 'NotProcess.p)
		  (set 'bptr 'NotProcess.p)]
		 [else
		  ;; otherwise, point to the next process
		  (set 'fptr (workspace front next-ws))])

		]

	       [(not (equal? timer 'NotTimerProcess.p))
		;;This is a busywait case. 
		
		
		(sleep sleep-duration)
		(checker)]
	       
	       [else
		;;Many kinds of badness. We fuckethed this up.
		(error "No. Bad. Process badness. You are so humped.")])))])
      
      ;;Looking for phantom ALT processes; see instruction DIST
      ;; in "Inside the Transputer", page 118 for more information. 
      ;; We are removing this process from the queue if 'Disabling.p
      ;; is set in (Wptr - 3) or ALT-STATE. Then, we'll run the scheduler
      ;; as normal.
      
      ;; matt thinks (get 'tptr) should be wptr, but can't defend why.

      ;;(printf "w ~a t ~a~n" (get 'wptr) (get 'tptr))
      (unless (equal? 'NotTimerProcess.p (get 'tptr))
	;;(printf "w ~a t ~a~n" (get 'wptr) (get 'tptr))
	(let loop ([this-alt-state (workspace (get 'tptr) alt-state)])
	  (if (equal? this-alt-state 'Disabling.p)
	      (begin
		(printf "--- HUMPED ---~n")
		(set 'tptr (workspace (get 'tptr) next-t))
		(set 'tnext (workspace (get 'tptr) timeout))
		(loop (workspace (get 'tptr) alt-state))
		))))
      (checker)
      )))
