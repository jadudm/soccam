
(define-syntax add-instruction
  (lambda (stx)
    (syntax-case stx ()
      [(_ key bodies ...)
       #`(hash-table-put!
	  instructions 
	  (quote key) 
	  (lambda () bodies ...))])))

(define-syntax add-instruction+
  (lambda (stx)
    (syntax-case stx ()
      [(_ key proc)
       #'(hash-table-put!
	  instructions 
	  (quote key) proc)])))

(define head? 
  (lambda (sym ls)
    (equal? sym (car ls))))


(define instruction-range?
  (lambda (i)
    (let ([n (- (quotient MEM-SIZE WORDSIZE)
		(length *instructions*))])
      (and 
       (>= i (resize n))
       (< i MEM-SIZE)))))


#|
(let [(orig-exception-handler (current-exception-handler))
      (show-help #t)]
  (current-exception-handler 
   (lambda (err)
     (letrec [(print-err (lambda() (printf (exn-message err))))]
       (current-exception-handler orig-exception-handler)
       (flush-output (current-output-port))
       (flush-output (current-error-port))
       (newline)
       (printf (exn-message err))
       (newline)
       (parameterize 
	([current-prompt-read 
	  (lambda ()
	    (cond 
	     [show-help
	      (set! show-help #f)
	      (printf "Type: (help) for help!~n")])
	    (printf "> ")
	    (read))])
	(read-eval-print-loop))
       (flush-output (current-output-port))
       (flush-output (current-error-port))
       (exit)))))
|#



(define show-instructions
  (lambda ()
    (let ([mem-loc (filter
		    instruction-range?
		    (quicksort (hash-table-map
				MEM 
				(lambda (k v) k))
			       <))])
      (for-each 
       (lambda (loc)
	 (printf "\t(~a)\t~a : ~a~n" (quotient loc WORDSIZE) loc 
		 (hash-table-get 
		  MEM
		  loc (lambda () (error "Nothing there.")))))
       mem-loc))))


(define show-label-table
  (lambda ()
    (hash-table-for-each
     label-table
     (lambda (k v)
       (printf "~a : ~a~n" k v)))))


(define show-memory
  (lambda ()
    (let ([mem-loc (filter
		    (if ;;*show-instructions*
		     #f
			(lambda (x) #t)
			(lambda (x) (not (instruction-range? x))))
		    (quicksort (hash-table-map
				MEM 
				(lambda (k v) k))
			       <))])
      (for-each 
       (lambda (loc)
	 (printf "\t(~a)\t~a : ~a~n" (quotient loc WORDSIZE) loc 
		 (hash-table-get 
		  MEM
		  loc (lambda () (error "Nothing there.")))))
       mem-loc))))

(define show-state
  (lambda ()
    (printf "Registers: ~n")
    (let ([regs 
	   (quicksort (hash-table-map 
		       *mach* 
		       (lambda (k v) 
			 (symbol->string k))) 
		      (lambda (s1 s2)
			(char<? (string-ref s1 3)
				(string-ref s2 3))))])
      (for-each
       (lambda (k)
	 (let ([val (hash-table-get 
		     *mach* 
		     (string->symbol k) 
		     (lambda () (error "No reg.")))])
	   (printf "\t~a <- ~a~n" k val)))
       regs))
    (newline)
    ))


(define show-timer-queue
  (lambda (current)
    (unless (not *debug*)
      (cond
       [(equal? 'NotTimerProcess.p current)
	(printf "[]~n")]
       [else
	(printf "[~a | ~a | ~a] -> "
		current
		(workspace current timeout)
		(workspace current next-t)
		)
	(show-timer-queue 
	 (workspace current next-t))]))))

(define insert-instruction
  (let ([current-loc 0]
	[data-flag #f])
    (lambda (instr)
      (cond
       [(head? 'run instr) '...]
       
       [(head? '.databytes instr)
	(set! data-flag #t)]

	[(and (four-split instr) data-flag)
	 (for-each
	  (lambda (group)
	    (let* ([group (map ->charint group)]
		   [new-word
		    (integer->integer-byte-string 0 WORDSIZE #t)]
		   [new-word-vec
		    (list->vector 
		     (map char->integer
			  (string->list new-word)))])

	      (let ([index 0])
		(for-each 
		 (lambda (byte)
		   (vector-set!  new-word-vec (- (sub1 WORDSIZE) index) byte)
		   (set! index (add1 index)))
		 group))
	    
	      (set! *instructions*
		    (reverse
		     (cons 
		      (integer-byte-string->integer
		       (list->string
			(map integer->char 
			     (vector->list new-word-vec)))
		       #t)
		       (reverse *instructions*))))
	      ))
	  (four-split instr))
	 (set! current-loc (add1 current-loc))
	 ]

	[else
	 (set! data-flag #f)
	 (set! *instructions* (reverse (cons instr (reverse *instructions*))))
	 (set! current-loc (add1 current-loc))
	 (if (member (car instr) '(label: .proc))
	     (begin
	       ;;(printf "Whoo! ~a ~a ~a~n" else current-loc (symbol? (cadr else))
	       (hash-table-put!
		label-table
		(cadr instr)
		current-loc)))
	 ]))))



(define get-label-location
  (lambda (lab)
    (hash-table-get label-table lab (lambda () 'bad-location))))

(define load-instructions
  (lambda (ls loc)
    (cond
     [(null? ls) 'Done]
     [else
      (write-mem! (resize loc) (car ls))
      (if (and 
	   (list? (car ls))
	   (member (caar ls) '(label: .proc)))
	  (hash-table-put!
	   label-table
	   (cadar ls) (resize loc)))
      (load-instructions (cdr ls) (add1 loc))])))
	


(define four-split
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(>= (length ls) 4)
       (cons (list (car ls)
		   (cadr ls)
		   (caddr ls)
		   (cadddr ls))
	     (four-split (cddddr ls)))]
      [else #f])))


(define ->charint
  (lambda (hex)
    (string->number (if (number? hex)
			(number->string hex)
			(symbol->string hex)) 16)))

;;queue
(define add-to-queue
  (lambda (source-reg iptr-prime)
    ;;Source reg is effectively the new current workspace pointer.
    (begin
      (if (equal? (get 'fptr) 'NotProcess.p)
	  ;; If there is nothing on the queue, 
	  ;; then we will make ourselves the only
	  ;; process on the queue.
	  (begin
	    (set 'fptr (get source-reg))
	    (set 'bptr (get source-reg)))
	  
	  ;; Otherwise, we aren't the only process on the queue.
	  ;; We add ourselves to the back pointer (end of the queue).
	  ;; We also add a pointer to ourselves in the previous 
	  ;; process's workspace.
	  (begin
	    (write-mem! (ptr- (get 'bptr) 2) (get source-reg))
	    (set 'bptr (get  source-reg))))
      ;;WARNING 20040218
      ;; Technically, we should leave the stack EMPTY
      ;; after this process. However, if we want to reuse it for
      ;; (startp), we have to leave the Creg in the Areg.
      ;; Dangerous? Possibly.
      (write-mem! (ptr- (get 'bptr) 1) iptr-prime)
      (stack! (get 'creg) undef undef)
       )))

;;HELPER PROCEDURES
;; Probably belong somewhere else.

(define BEFORE? 
  (lambda (t1 t2)
    (let ([comp (if (or 
		     (and (negative? t1)
			  (negative? t2))
		     (and (positive? t1)
			  (positive? t2))
		     (and (negative? t1)
			  (negative? t2))
			  
		     )
		    <
		    ;; The else is for when we wrap around from 
		    ;; positive to negative.
		    >=)])
      (comp t1 t2))))

;;EG
;; (BEFORE? -10 -5) => #t 
;; Because time is increasing from more negative to less negative, therefore
;; -10 comes before -5
;;
;; (BEFORE? -5 10) => #t
;; Because negative time rolls through zero into positive time.
;;
;; (BEFOFRE? 10 -5) => #t
;; Because we would have rolled over the top of the timer, and 
;; into negative numbers. For *very* long amounts of time, the
;; logic encoded in BEFORE? will break. We need to fix it properly.


(define AFTER?
  (lambda (t1 t2)
    (not (BEFORE? t1 t2))))

(define traverse-and-insert
  (lambda (a-wptr prev-wptr)
    (let ([empty-timer-queue? 
	   (lambda (s) (equal? 'NotTimerProcess.p s))])
      
      (cond
       [(and (empty-timer-queue? a-wptr)
	     (empty-timer-queue? prev-wptr))
	(set 'tptr (get 'wptr))
	(set 'tnext (workspace wptr timeout))
	(workspace! wptr iptr (get 'iptr))
	(workspace! wptr next-t 'NotTimerProcess.p)
	;;(show-timer-queue (get 'tptr))
	]
       
       [else
	;;everything else

	

	
	(let ([next-wptr
	       (workspace a-wptr next-t)]
	      [this-rtime
	       (workspace a-wptr timeout)])

	  (cond

	   ;;END OF QUEUE
	   [(equal? 'NotTimerProcess.p 
		    (workspace a-wptr next-t))
	    (workspace! a-wptr next-t (get 'wptr))
	    (workspace! wptr next-t 'NotTimerProcess.p)
	    (show-timer-queue (get 'tptr))
	    ]
	   
	   ;;WE NEED TO BE THE TOP
	   [(BEFORE? reschedule-time (get 'tnext))
	    
	    ;;(printf "~nHere.~n")
	    (workspace! wptr next-t a-wptr)
	    
	    ;;We're at the top now; update tptr and 
	    ;; the associated reschedule time.
	    (set 'tptr (get 'wptr))
	    (set 'tnext reschedule-time)
	    (show-timer-queue (get 'tptr))
	    ;;(printf "~nEverywhere.~n")
	    ]
	   
	   ;;WE ARE THE BOMB
	   [(BEFORE? reschedule-time this-rtime)
	    ;;Insert if we are less
	    ;; than this other guy.

	    
	    (workspace! wptr next-t a-wptr)
	    (workspace! prev-wptr next-t (get 'wptr))
	    (show-timer-queue (get 'tptr))
	    ]
	   
	   [else
	    
	    ;;Otherwise, keep traversing the list
	    (show-timer-queue (get 'tptr))
	    (traverse next-wptr a-wptr)]
	   ))] ;;end else
       );;end outer cond
      )))
