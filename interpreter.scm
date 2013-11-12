#!/usr/local/bin/mzscheme -gqr

(require (lib "list.ss")
         (lib "defmacro.ss")
	 "convert2.ss")


(read-case-sensitive #f)

(define *DEBUG* #f)

(define basedir "~/Documents/dev/occam/v2/")
(load (string-append basedir "machine.scm"))
(load (string-append basedir "helpers.scm"))
(load (string-append basedir "scheduler.scm"))
(load (string-append basedir "memory.scm"))
(load (string-append basedir "Primary.scm"))
(load (string-append basedir "Secondary.scm"))
(load (string-append basedir "Channel.scm"))
(load (string-append basedir "Alt.scm"))
(load (string-append basedir "Timer.scm"))
(load (string-append basedir "Fred.scm"))
(load (string-append basedir "Clarity.scm"))

(define execute
  (lambda (instr hash)
    (apply 
     (hash-table-get
      hash (car instr) 
      (lambda () 
	(error (format "No instruction : ~a~n" instr)))) 
     (cdr instr))))


(define debug-execute
  (lambda (instr hash)
    (apply 
     (hash-table-get
      hash (car instr) 
      (lambda () 
	(error (format "No instruction : ~a~n" instr)))) 
     (cdr instr))
    
    (accounting (car instr))

    (if *DEBUG*
	(begin
	  (printf "== ~a ==~n" instr)
	  (show-state)
	  (show-memory)))
    ))

(define accounting
  (let ([h (make-hash-table)])
    (case-lambda
     [()
      (let ([sum 0])
	(hash-table-for-each
	 h (lambda (k v) (set! sum (+ sum v))))
	(printf "%\t#\tInst~n-------------~n")
	(let ([ls
	       (quicksort
		(hash-table-map
		 h (lambda (k v) (list
				  (floor (* 100 (/ v sum))) 
				  v
				  k)))
		(lambda (ls1 ls2)
		  (> (car ls1) (car ls2))))])
	  (for-each
	   (lambda (ls)
	     (printf "~a\t ~a\t ~a~n"
		     (car ls) (cadr ls) (caddr ls)))
	   ls)))]
     [(inst)
      (hash-table-put! 
       h inst (add1 
	       (hash-table-get 
		h inst (lambda () 0))))])))

(define dispatch
  (lambda (instr)
    (NEXT-INSTRUCTION)
    (debug-execute instr instructions)))

(define (NEXT-INSTRUCTION)
  (set 'iptr (ptr+ (get 'iptr) 1)))


(define run
  (lambda (lab)
    (set 'iptr (get-label-location lab))
    (let loop ([current-inst (get 'iptr)]
	       [count 0])
      (let ([cinst (read-mem (get 'iptr))])
	(dispatch cinst)
	(if *DEBUG*
	    (printf "Inst # ~a~n" count))
	
	;;Timer shagged?
	(if (and 
	     (number? (get 'tptr))
	     (not (= (get 'tnext)
		     (workspace (get 'tptr) timeout))))
	    (error "Shagged."))
	
	;;iptr shagged?
	(if (not (number? (get 'iptr)))
	    (error "Iptr shagged."))
	
	(if (instruction-range? (get 'iptr))
	    (loop (get 'iptr) (add1 count))
	    (begin
	      (printf "Instructions : ~a~n" count)
	      (accounting)))
	))
      ))



(define load-and-run
  (let* ([DO-MAGIC
	  (lambda ()
	    ;; Make space to insert the instructions
	    (mem-size! (+ (quotient MEM-SIZE WORDSIZE) (length *instructions*)))
	    ;;Insert the instructions into main memory
	    (load-instructions *instructions* 
			       (- (quotient MEM-SIZE WORDSIZE)
				  (length *instructions*)))
	    ;;Since call will save the stack, we're going to preload the stack
	    ;; with values for the kyb, scr, and err channels.
	    (stack! 'kyb 'scr 'err))]
	 [branch
	  (lambda (instr)
	    (cond
	     [(head? 'init instr) (init)]
	     [(head? 'mem  instr) (mem-size! (cadr instr))]
	     [(head? 'run  instr)
	      (DO-MAGIC)
	      ;;Lastly, start running the program.
	      (let ([start (current-milliseconds)])
		(run (cadr instr))
		(printf "Run time: ~a ms~n" (- (current-milliseconds)
					    start))) ]
	     [else
	      (insert-instruction instr)]))])
    (lambda (program)
      (cond
       [(null? program) '()]
       [else
	(branch (car program))
	(load-and-run (cdr program))]))
    ))


(define argv (vector "tests/dmemtest.etc"))

(define launch
  (lambda (argv)
    (unless (> 1 (vector-length argv))
      (let* ([file (vector-ref argv 0)]
             [start-load (current-seconds)]
             [code-read (convert file)]
             [end-load (current-seconds)])
        (load-and-run code-read)))))

(launch argv)




#|
(hash-table-put! instructions 'test (lambda () (printf "Testing!~n")))
(add-instruction+ square 
		  (lambda (x) (* x x)))
(add-instruction test2
  (printf "This is a test~n")
  (printf "And another test.~n"))

(dispatch '(test))

(display (dispatch '(square 12)))
(newline)

(dispatch '(test2))
|#

