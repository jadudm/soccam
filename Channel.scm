;; WARNING 20040218
;; Both (in) and (out) are interruptable. This
;; will likely never be implemented in our interpreter.
;; Not for a while, anyway. Christian claims it's easy. 
;; I doubt it. I suspect tedium.
(add-instruction in
  (let ([num-of-bytes (get 'areg)]
	[channel      (get 'breg)]
	[write-start    (get 'creg)])
    (cond
     [(equal? (read-mem channel) 'NotProcess.p)
      (write-mem! channel (get 'wptr))
      (workspace! wptr channel write-start)
      ;; Storing the iptr is taken care of by 'add-to-queue'
      ;; Nope. But, we might change our minds again.
      (workspace! wptr iptr (get 'iptr))
      (run-next-on-queue) ]
     
     ;;Otherwise, we can read some data.
     [else
      (let ([read-start (read-mem channel)])
	(let loop ([bnum 0])
	  (unless (= bnum num-of-bytes)
	    (let ([byte-read (read-byte read-start bnum)])
	      (write-byte! write-start bnum byte-read)
	      (loop (add1 bnum))))))
      
      ;; Add ourselves back to the queue
      (add-to-queue 'wptr (get 'iptr))
      ;; Reschedule the other process
      ;; Meaning, run them. We aren't running 
      ;; from the queue (eg. 'run-next-on-queue'),
      ;; but instead running this *particular* next process.
      (set 'wptr (read-mem channel))
      (set 'iptr (workspace wptr iptr)) 
      ;; Set the channel word to NotProcess.p
      (write-mem! channel 'NotProcess.p) ])
    ;; END OF COND

    (stack! undef undef undef)
	 ))


;; TO-DO 20040218
;; The first and else cases of the cond are (effectively) identical
;; for (in) and (out). (out) has some different cases for ALTernation.
;; Still, there is code duplication---just watch the variable switch
;; in the else case (the read-start and write-start variables both come
;; from different places).
(add-instruction out
  (let ([num-of-bytes  (get 'areg)]
	[chan-ptr      (get 'breg)] ;; 16 
	[read-start    (get 'creg)]) ;; 3 

    (cond
     [(member chan-ptr '(scr err))
      (let ([outp (if (equal? chan-ptr 'scr)
		      (current-output-port)
		      (current-error-port))])
	(if (> num-of-bytes 1)
	    (error 
	     (format "Oops! Trying to output ~a bytes to ~a" 
		     num-of-bytes chan-ptr))
	    ;;otherwise, print one byte
	    (let ([to-be-output
		   (read-byte read-start)])
	      (if (= to-be-output 255)
		  (flush-output outp)
		  (fprintf 
		   outp 
		   (format "~a" (integer->char to-be-output)))))))
	   
      ;; WARNING 20040225
      ;; It may be, when blasting things to the screen,
      ;; that we need to reschedule things so other people
      ;; can get in between our slow printing process.
      ;; (add-to-queue wptr (get 'iptr))
      ;; (run-next-on-queue)
      ]
	  
     [(equal? (read-mem chan-ptr) 'NotProcess.p)
      (write-mem! chan-ptr (get 'wptr))
      (workspace! wptr iptr (get 'iptr))
      (workspace! wptr channel read-start)
      (run-next-on-queue) ]
     
     [(and 
       (not (equal? (read-mem chan-ptr) 'NotProcess.p))
       (equal? (workspace (read-mem chan-ptr) alt-state) 
	       'Enabling.p))

      (workspace! (read-mem chan-ptr) alt-state 'Disabling.p)
      (workspace! chan-ptr top (get 'wptr))
      (workspace! wptr iptr (get 'iptr))
      (workspace! wptr alt-state read-start)
      (run-next-on-queue) ]

     [(and (not (equal? (read-mem chan-ptr) 'NotProcess.p))
	   (equal? (workspace (read-mem chan-ptr) alt-state) 
		   'Waiting.p))
      (printf "==== A~n")
      ;;Need to swap before we obliterate things in memory.
      (let ([old-chan-word (read-mem chan-ptr)])
	(workspace! (read-mem chan-ptr) alt-state 'Disabling.p)
	(workspace! chan-ptr top (get 'wptr))
	(workspace! wptr iptr (get 'iptr))
	(workspace! wptr channel read-start) 
	(add-to-queue 'wptr (get 'iptr))
	(printf "==== B~n")
	(set 'wptr old-chan-word)
	(set 'iptr (workspace wptr iptr))
	(printf "iptr : ~a~n" (get 'iptr))
	)
      ]

     [(and (not (equal? (read-mem chan-ptr) 'NotProcess.p))
		(equal? (workspace (read-mem chan-ptr) alt-state) 
			'Disabling.p))
	   
      (workspace! chan-ptr top (get 'wptr))
      (workspace! wptr iptr (get 'iptr))
      (workspace! wptr channel read-start)
      (run-next-on-queue)]

     [else
      ;;WARNING 20040218
      ;; There are two differences here between (in) and (out).
      ;; The most important is the memory location from which we read
      ;; or write is different in each case. In (out), it is 
      ;; (- (read-mem chan-ptr) 3), whereas (in) just reads from 
      ;; channel (or the Breg).
      
      (let ([write-start (workspace (read-mem chan-ptr) channel)])
	(let loop ([bnum 0])
	  (unless (= bnum num-of-bytes)
	    (let ([byte-read (read-byte read-start bnum)])
	      (write-byte! write-start bnum byte-read)
	      (loop (add1 bnum))))) )

      ;; Add ourselves back to the queue
      (add-to-queue 'wptr (get 'iptr))
      
      ;; Reschedule the other process
      ;; Meaning, run them. We aren't running 
      ;; from the queue (eg. 'run-next-on-queue'),
      ;; but instead running this *particular* next process.
      (set 'wptr (read-mem chan-ptr))
      (set 'iptr (workspace wptr iptr))
      ;; Set the channel word to NotProcess.p
      (write-mem! chan-ptr 'NotProcess.p) 
      ] 
     ;;end else
     ))
       
  (stack! undef undef undef))


(add-instruction outbyte
  (write-mem! (get 'wptr) (get 'areg))
  (stack! 1 b (get 'wptr))
  (set 'iptr (ptr- (get 'iptr) 1))
  (if *DEBUG* 
      (begin
	(printf "\t\t\t\t===> outbyte: pre-out.~n")
	(show-state)
	(show-memory)))
  (dispatch '(out)))
  ;;(ChannelInstr '(out)))

(add-instruction outword
  (write-mem! (get 'wptr) (get 'areg))
  (stack! WORDSIZE b (get 'wptr))
  (set 'iptr (ptr- (get 'iptr) 1))
  (if *DEBUG* 
      (begin
	(printf "\t\t\t\t===> outword: pre-out.~n")
	(show-state)
	(show-memory)))
  (dispatch '(out)))
  ;;(ChannelInstr '(out)))
