(define *mach* (make-hash-table))
(define label-table (make-hash-table))
;; This temporarily holds the instructions
;; while they are read in from a file.
(define *instructions*  '())
;;This is the hash that we use to look up instructions while
;; actually running.
(define instructions (make-hash-table))
(define MEM (make-hash-table))
(define MEM-SIZE 0)
(define VECTORSPACE #f)
(define MOBILESPACE #f)
(define MinInt (expt -2 15))
(define MostNeg MinInt)
(define MostPos (expt 2 15))

(define WORDSIZE 4)

(define resize
  (lambda (n)
    (arithmetic-shift n (arithmetic-shift WORDSIZE -1))))


(define ptr+
  (lambda (ptr n)
    (+ ptr (resize n))))

(define ptr-
  (lambda (ptr n)
    (- ptr (resize n))))

(define ptr1
  (lambda (ptr)
    (ptr+ ptr 1)))

(define init
  (lambda ()

    ;;REGISTERS
    (hash-table-put! *mach* 'areg 'Undef)
    (hash-table-put! *mach* 'breg 'Undef)
    (hash-table-put! *mach* 'creg 'Undef)
    ;;(hash-table-put! *mach* 'Oreg 'Undef)
    (hash-table-put! *mach* 'tnext 'Undef)
    (hash-table-put! *mach* 'error-flag #f)

    ;;POINTERS
    ;; NOTE 20040215
    ;; We're subtracting one from the length of the mem
    ;; because Scheme vectors are index from 0 ... (- n 1), whereas
    ;; the ETC code will be indexing from 1 ... n. Therefore, we 
    ;; start our mem pointer with an offset of -1.
    ;; 
    ;; Same for the iptr.
    (hash-table-put! *mach* 'wptr MEM-SIZE)
    (hash-table-put! *mach* 'iptr MEM-SIZE)
    ;; tptr: Pointer to the mems on the queue, 
    ;; currently timed out, waiting to run. 
    (hash-table-put! *mach* 'tptr 'NotTimerProcess.p) 
    ;; fptr, bptr: Pointers to the front and back
    ;; of the scheduling queue
    (hash-table-put! *mach* 'fptr 'NotProcess.p)
    (hash-table-put! *mach* 'bptr 'NotProcess.p)
   
    ))	 

(define set
  (lambda (reg val)
    (hash-table-put! *mach* reg val)))
(define get
  (lambda (reg)
    (hash-table-get 
     *mach* reg
     (lambda () (error (format "Can't get ~a" reg)))) ))
(define move
  (lambda (src tgt)
    (hash-table-put! *mach* tgt (get src))))
(define undef
  (lambda (reg)
    (hash-table-put! *mach* reg (void))))


(define-macro workspace
  (lambda (pointer sym)
    `(let ([ptr (if (member (quote ,pointer) '(wptr fptr bptr areg breg creg))
		    (get (quote ,pointer))
		    ,pointer)]
	   [check? (lambda (s) (equal? (quote ,sym) s))])
    (cond
	[(check? 'top)     (read-mem ptr)]
	[(check? 'iptr)    (read-mem (ptr- ptr 1))]
	[(check? 'next-ws) (read-mem (ptr- ptr 2))]
	[(member (quote ,sym) '(channel alt-state)) (read-mem (ptr- ptr  3))]
	[(member (quote ,sym) '(next-t alt-t))  (read-mem (ptr- ptr 4))]
	[(check? 'timeout) (read-mem (ptr- ptr 5))]
	[else (error "Invalid workspace location")]))))

(define-macro workspace!
  (lambda (pointer sym val)
    `(let ([ptr (if (member (quote ,pointer) '(wptr fptr bptr areg breg creg))
		    (get (quote ,pointer))
		    ,pointer)]
	   [check? (lambda (s) (equal? (quote ,sym) s))])
       (cond
	[(check? 'top)     (write-mem! ptr ,val)]
	[(check? 'iptr)    
	 (if (not (number? ,val))
	     (error (format "Attempting to store ~a into Iptr." ,val))
	     (write-mem! (ptr- ptr 1) ,val))]
	[(check? 'next-ws) (write-mem! (ptr- ptr 2) ,val)]
	[(member (quote ,sym) '(channel alt-state)) (write-mem! (ptr- ptr 3) ,val)]
	[(member (quote ,sym) '(next-t alt-t))  (write-mem! (ptr- ptr 4) ,val)]
	[(check? 'timeout) (write-mem! (ptr- ptr 5) ,val)]
	[else (error "Invalid workspace location")]))))


(define-macro stack!
  (lambda (aprime bprime cprime)
    (let ([symbols '(a b c undef)])
      `(let ([a (get 'areg)]
	     [b (get 'breg)]
	     [c (get 'creg)]
	     [undef 'undef])
	 ,@(map (lambda (reg var)
		  `(set (quote ,reg) ,var))
		'(areg breg creg)
		(list aprime bprime cprime)))) ))


(define write-mem!
  (lambda (loc value)
    (cond
     [(or (> loc MEM-SIZE)
	  ;;NOt with dyn memory
          ;;(< loc 0)
          )
      (error 
       (format "Out of bounds; Attempted to write (~a) to location (~a)~n"
	       value loc))]
     [(zero? (remainder loc WORDSIZE))
      (hash-table-put!
       MEM
       loc value)]
     [else
      (error 
       (format
	"Attempt to write (~a) off boundary to location (~a)~n"
	value loc))])))


(define read-mem
  (lambda (loc)
    (cond
     [(or (> loc MEM-SIZE)
	  ;;Not no more!
          ;;(< loc 0)
          )
      (error 
       (format "Out of bounds; Attempted to read from location (~a)~n" loc))]
     [(not (zero? (remainder loc WORDSIZE)))
      (error 
       (format
	"Attempt to read memory off word boundary.~nAddress: ~a~n"
	loc))]
     [else
      (hash-table-get
       MEM
       loc (lambda () (void)))])
    ))



(define read-byte
  (case-lambda
   [(base index) 
    (let* ([word-value (read-mem (* WORDSIZE (quotient base WORDSIZE)))]
	   ;; The '4' is the number of bytes in a word; this 
	   ;; is part of where we start having to choose memory rep, etc.
	   [str (integer->integer-byte-string word-value WORDSIZE #t)]

	   [res 
	    (if (system-big-endian?)
		;; BIG ENDIAN
		(char->integer (string-ref str (- (sub1 (string-length str)) index)))
		;; LITLE ENDIAN
		(char->integer (string-ref str (+ index))))])
      ;; (prinf "read byte : ~a~n" res)
      ;;(debug-mem "RB: (~a.~a) ~a~n" base index res)
      res
      )]
   [(base) 
    (let* ([index (abs (remainder base WORDSIZE))])
      (read-byte base index))]))

(define write-byte! 
  (case-lambda
    [(base index value)
     (let ([baseize (lambda (b) (* WORDSIZE (quotient b WORDSIZE)))])
       (if (< value (expt 2 8))
           (let* ([mem-loc-val (if (not (number? (read-mem (baseize base))))
                                   0 
                                   (read-mem (baseize base)))]
                  [current-word (integer->integer-byte-string mem-loc-val WORDSIZE #t)]
                  [word-vec (list->vector 
                             (map char->integer
                                  (string->list current-word)))])
             
             (if (system-big-endian?)
                 ;; BIG ENDIAN
                 (vector-set!  word-vec (- (sub1 WORDSIZE) index) value)
                 ;; LITLE ENDIAN
                 (vector-set!  word-vec (+ index) value))
             
             (write-mem! (baseize base)
                         (integer-byte-string->integer
                          (list->string
                           (map integer->char 
                                (vector->list word-vec)))
                          #t
                          )))
           (error "Tried to put too many bits in a byte.")))]
    [(base value)
     (let ([index (abs (remainder base WORDSIZE))])
       ;;(debug-mem "base: ~a\tindex: ~a\tvalue: ~a\t~n" base  index value)
       (write-byte! base index value))]))
     


(define mem-size!
  (lambda (size)
    (printf "Setting mem-size to ~a~n" (resize size))
    (set! MEM-SIZE (resize size))))
