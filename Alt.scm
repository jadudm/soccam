(add-instruction alt
  (workspace! wptr alt-state 'Enabling.p))


(add-instruction altend
  (set 'iptr (+ (get 'iptr) (workspace wptr top))))

      
(add-instruction altwt
  (if (equal? (workspace wptr alt-state)  'Disabling.p)
      (begin
	(workspace! wptr top 'NoneSelected.o)
	(void))
      (begin
	(workspace! wptr alt-state 'Waiting.p)
	(workspace! wptr iptr (get 'iptr))
	(workspace! wptr top 'NoneSelected.o)
	;; WARNING 2004024
	;; Currently, it is 68% likely we actually 
	;; DO need to reschedule here. Matt is not sure, Christian
	;; is mostly sure.
	(run-next-on-queue)
	))
  (stack! undef undef undef))


(add-instruction disc
  (let* ([breg (get 'breg)]
	 [w-deref (workspace wptr top)]
	 [creg-deref (read-mem (get 'creg))]
	 [fired? (and (not (= breg 0))
		      (equal? w-deref 'NoneSelected.o)
		      (not (equal? creg-deref 'NotProcess.p))
		      (not (= creg-deref (get 'wptr)))
		      )])
    (if fired? (workspace! wptr top (get 'areg)))
    
    ;;Additionally
    ;; If this test is true, then it implies we are looking
    ;; at ourselves. So, we should destroy evidence of 
    ;; a waiting process (US!) before doing everything else.
    (if (and (not (zero? breg))
	     (= creg-deref (get 'wptr)))
	(write-mem! (get 'creg) 'NotProcess.p))
	 
    (stack! (if fired? 1 0) undef undef)))
      
(add-instruction diss
  (let ([fired? (and (not (zero? (get 'breg)))
		     (equal? (workspace wptr top) 'NoneSelected.o))])
    
    (if fired? (workspace! wptr top (get 'areg)))
    (stack! (if fired? 1 0) c undef)))
      
(add-instruction dist
  (let* ([current-time (current-milliseconds)]
	 [fired? (and
		  (and (not (zero? (get 'breg)))
		       (equal? (workspace wptr top) 'NoneSelected.o))
		  (AFTER? current-time (get 'creg)))])
    
    (if fired? (workspace! wptr top (get 'areg)))
    
    ;;Additionally
    (if (and (not (zero? (get 'breg)))
	     (BEFORE? current-time (get 'creg))
	     (not (equal? (get 'tptr) 'NotTimerProcess.p)) )
	(let loop ([wptr (get 'tptr)]
		   [next (workspace (get 'tptr) next-t)]
		   [prev #f])
	  (cond
	   ;;HEAD CHECK?
	   ;; We're at the head of the list
	   [(and (= wptr (get 'tptr))
		 (= wptr (get 'wptr)))
	    ;;We will either have someone after us, or noone.
	    (cond
	     ;; If there is noone after us, then we can just
	     ;; blast the tptr.
	     [(equal? next 'NotTimerProcess.p)
	      (set 'tptr 'NotTimerProcess.p)]
	     ;; Otherwise, we need to put them at the head
	     ;; of the queue, and also set the timeout to their
	     ;; timeout value.
	     [else
	      (set 'tptr (workspace wptr next-t))
	      (set 'tnext (workspace (get 'tptr) timeout))])]
	   
	   ;;TERMINATION
	   ;; If we find ourselves, then do some manipulation
	   ;; to the list, and get out.
	   [(= wptr (get 'wptr))
	    ;;Set the previous process's next to '()
	    (workspace! prev next-t (workspace wptr next-t))]
	   
	   ;; AT END OF THE LIST
	   ;; Do nothing
	   [(equal? next 'NotTImerProcess.p)
	    (void)]
	   
	   ;; KEEP LOOKING
	   [else
	    (loop 
	     ;; new wptr
	     (workspace wptr next-t)
	     ;; new next
	     (workspace (workspace wptr next-t) next-t)
	     ;; new prev
	     wptr)])))
    
    (stack! (if fired? 1 0) undef undef)
    ))


(add-instruction enbc
  (if (= (get 'areg) 0) 
      ;;Guard not enabled, do nothing.
      (void)
      ;;otherwise...
      (cond
       [(equal? (read-mem (get 'breg)) 'NotProcess.p)
	;;No process waiting on the channel, so init communication
	(write-mem! (get 'breg) (get 'wptr))]
       [else
	(cond
	 [(equal? (read-mem (get 'breg)) (get 'wptr))
	  ;;Another guard of the current process is waiting
	  ;; on the channnel; do nothing.
	  (void)]
	 [else
	  ;;another process is waiting on the channel, so set a 
	  ;; flag to show that the guard is ready
	  (workspace! wptr alt-state 'Disabling.p)]
	 )]))
  
  ;;Oops! Don't forget, we have to ... change the stack.
  (stack! a c undef))


(add-instruction enbs
  ;;The stack is unaffected by this instruction
  (if (= (get 'areg) 1)
      (workspace! wptr alt-state 'Disabling.p)))


(add-instruction enbt
  (cond
   [(= (get 'areg) 0)
    ;;Do nothing
    (stack! a c undef)
    (void)]
   [else
    (cond
     [(equal? (workspace wptr alt-t) 'TimeNotSet.p)
      (workspace! wptr alt-t 'TimeSet.p)
      (workspace! wptr timeout (get 'breg))]
     [(AFTER? (workspace wptr timeout) (get 'breg))
      (workspace! wptr timeout (get 'breg))])])
  (stack! a c undef))

      
(add-instruction talt
  (workspace! wptr alt-state 'Enabling.p)
  (workspace! wptr alt-t 'TimeNotSet.p))

(add-instruction taltwt
  (let ([current-time (current-milliseconds)])
	 (cond
	  [(or 
	    (equal? (workspace wptr alt-state) ;;'Enabling.p)
		    'Disabling.p)
	    (and (equal? (workspace wptr alt-t) 'TimeSet.p)
		 (BEFORE? (workspace wptr timeout) current-time)))
	   ;;Undef the stack, blast the top of the workspace
	   
	   
	   
	   (stack! undef undef undef)
	   (workspace! wptr top 'NoneSelected.o)
	   ;; And do nothing.
	   (void)]

	  
	  [(equal? (workspace wptr next-t) 'TimeNotSet.p)
	   (workspace! wptr alt-state 'Waiting.p)
	   (stack! undef undef undef)
	   (run-next-on-queue)]


	  [else
	   
	   ;;Insert ourselves
	   ;;(printf "~n\tHere~n")
	   (traverse-and-insert (get 'tptr) (get 'tptr))
	   (workspace! wptr alt-state 'Waiting.p)
	   (stack! undef undef undef)
	   (workspace! wptr top 'NoneSelected.o)
;;							(show-state)
;;							(error)
	   (run-next-on-queue)
	   ])))
