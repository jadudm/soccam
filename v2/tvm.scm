#!/usr/local/bin/mzscheme -gqr

(require (prefix list: (lib "list.ss"))
         (lib "1.ss" "srfi")
         (lib "cmdline.ss")
         "word-arithmetic.ss"
         "macros.ss")

(read-case-sensitive #f)

;; MACHINE DEFINITION
;; The registers hash contains the Transputer
;; registers: areg, breg, creg, wptr, tptr,
;; tnext, fptr, bptr.
;;
;; This table should only be accessed via the 
;; 'set-register!' and 'get-register' proectures.
(define *registers*    (make-hash-table))

;; The memory vector is our abstraction for memory.
;; Each location in the vector represents one byte of 
;; memory; all accesses should be through the 
;; 'read-word', 'write-word!', 'read-byte', and 'write-byte!'
;; instructions.
(define MEM (void))

;; The instructions vector contains the actual instructions
;; executed by the Transterpreter. Each vector location
;; contains a Scheme thunk, which is referenced out and 
;; invoked.
(define *instructions* (make-hash-table))



(define ptr+
  (lambda (ptr n)
    (word->integer 
     (word+ (word ptr) 
            ;;Used to be (resize n)
            (word (resize n))))))

(define ptr-
  (lambda (ptr n)
    (ptr+ ptr (* -1 n))))

(define ptr1
  (lambda (ptr)
    (ptr+ ptr 1)))



(define set-register!
  (lambda (reg val)
    (hash-table-put! *mach* reg val)))
(define get-register
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



(define write-mem!
  (lambda (loc value)
    (cond
     [(or (> loc MEM-SIZE)
	  ;;NOt with dyn memory
          (< loc 0)
          )
      (error 
       (format "Out of bounds; Attempted to write (~a) to location (~a)~n"
	       value loc))]
     [(zero? (remainder loc WORDSIZE))
      ;;(printf "Writing word ~a to ~a~n" value loc)
      (debug 
       (printf ":: ~a <- ~a~n" loc value))
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
      (debug 
       (show-state)
       (show-memory))
      (error 
       (format "Out of bounds; Attempted to read from location (~a) of ~a~n" 
               loc MEM-SIZE))]
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
      ;;(prinf "read byte : ~a~n" res)
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
             ;;(printf "CW: ~s~nWV: ~s~n" current-word word-vec)
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











(define MEM-SIZE 0)
(define VECTORSPACE #f)
(define MOBILESPACE #f)
(define MinInt (expt -2 31))
(define MostNeg MinInt)
(define MostPos (expt 2 32))

(define WORDSIZE 4)

(define NotProcess.p   #x80000000)
(define Enabling.p     #x80000001)
(define Waiting.p      #x80000002)
(define Disabling.p    #x80000003)
(define TimeSet.p      #x80000001)
(define TimeNotSet.p   #x80000002)
(define NoneSelected.o #xFFFFFFFF)
(define sleep-duration 1)



;                                            
;                                            
;                                            
;   ;;;;;                     ;              
;   ;    ;                    ;              
;   ;    ;                    ;              
;   ;    ;   ;;;   ;;;     ;; ;    ;;;   ; ; 
;   ;   ;   ;   ; ;   ;   ;  ;;   ;   ;  ;;  
;   ;;;;   ;    ;     ;  ;    ;  ;    ;  ;   
;   ;  ;   ;;;;;;  ;;;;  ;    ;  ;;;;;;  ;   
;   ;   ;  ;      ;   ;  ;    ;  ;       ;   
;   ;    ;  ;     ;   ;   ;  ;;   ;      ;   
;   ;     ;  ;;;;  ;;;;;   ;; ;    ;;;;  ;   
;                                            
;                                            
;    

;;CURRENT VERSION
(define *VERSION* 1)

(define parse-command-line
  (lambda ()
    (command-line 
     "tvm" (current-command-line-arguments)
     (once-each
      #| form is
      ("one-char-sym" "long-sym")
      variables 
      "help message"
      scheme code
      |#
      [("-d" "--debugging")
       debug-flag
       "\n\tSet the debug flag (0 or 1)."
       (if (zero? debug-flag)
	   (set! *DEBUG* #f)
	   (set! *DEBUG* #t))]
      [("-e" "--external-channels")
       ext-chan
       "\n\tLoad the external channel definitions."
       (load ext-chan)]
      )
     (args 
      (filename)
      (main filename))
     )))
(define (test fname)
  (main fname))

(define main
  (let ()
    
    (define bad-header?
      (lambda (file)
        (let ([header (get-header file)])
          (not (equal? "tvm" header)))))
    
    (define get-header
      (lambda (file)
        (list->string
         (map integer->char 
              (map (lambda (index) 
                     (list-ref file index))
                   '(0 1 2))))))

    ;;WARNING 20040815 
    ;; Currently, we won't check the version.
    (define wrong-version? 
      (lambda (file)
        (let ([version (get-version file)])
          (not (= *VERSION* version)))))
    
    (define get-version
      (lambda (file)
        (let ([v (list-ref file 3)])
          #|(bits->integer
           (word->bits
            (char->integer v))|#
          v)))
    
    ;; Currently, the header is 4 bytes.
    (define header-bytes 4)
    (define just-bytes
      (lambda (file)
        (letrec ([helper
                  ;;This is dying for an escape continuation
                  (lambda (ls hb)
                  (cond
                    [(null? ls) '()]
                    [(>= hb header-bytes)
                     (cons (car ls)
                           (helper (cdr ls) (add1 hb)))]
                    [else
                     (helper (cdr ls) (add1 hb))]))])
          (helper file 0))))
                     
 
    (define slurp
      (lambda (port)
        (let ([f '()])
          (let loop ([c (read-char port)])
            (unless (eof-object? c)
              (set! f (cons c f))
              (loop (read-char port))))
          (map char->integer (reverse f)))))
    
    (define inner-main
      (lambda (filename)
        (let ([file (slurp (open-input-file filename 'binary))])
          (cond 
            [(bad-header? file)
             (error (format "Bad header on ~a:~n\t Found ~a, expected \"TVM\"~n"
                            filename (get-header file)))]
            [(wrong-version? file)
             (error (format "Wrong version: ~s, expected ~s~n" 
                            (get-version file)
                            *VERSION*))]
            [else
             (run (just-bytes file))]))))
             
    (lambda (filename)
      (inner-main filename))))
     

#|
(printf "File:~n~a~n" 
             (map (lambda (i)
                    (format "~a"
                            (number->string i 16)))
                  (map char->integer byte-ls)))
|#
    
(define lookup-instruction-symbol
  (lambda (type loc)
    (let ([ils (if (equal? type 'primary)
                   primary-instruction-symbol-list
                   secondary-instruction-symbol-list)])
      (if (>= loc (length ils))
        (begin
          (debug 
           (show-state)
           (show-memory))
          (error (format "No ~a instruction at location ~a~n" type loc)))
        (let ([sym (list-ref ils loc)])
          sym)))))

(define lookup-instruction-thunk
  (lambda (sym)
    (hash-table-get 
       instructions sym 
       (lambda ()
         (error 
          (format "~a (~a) :: Instruction undefined." 
                  sym (->hex (get 'oreg)))))
       )))
          

(define INSTRUCTION-COUNT -1)
(define run 
  (lambda (byte-ls)
    (let ([offset (length byte-ls)]
          [SIZE-PASSED-IN 1200]
          [inst-length-in-words
           (let ([whole-words (quotient (length byte-ls) WORDSIZE)]
                 [rem (if (not 
                                 (zero? 
                                  (remainder (length byte-ls) WORDSIZE)))
                                1 0)])
             (+ whole-words rem))])
      ;;(printf "ISize: ~a~nLeng: ~a~nInst: ~a~n" inst-length-in-words (length byte-ls) byte-ls)
              
      (mem-size! (+ SIZE-PASSED-IN inst-length-in-words))
      ;; Load instructions starting at 0
      (load-instructions byte-ls 0)
      ;; Set the workspace ptr to the top
      ;; of the instructions, as well as rest of machine
      (init)
      (set-wordsize-constants)
      (set 'wptr (ptr- (get 'wptr) 4))
      (write-mem! (get 'wptr) 'finished)
      (write-mem! (ptr+ (get 'wptr) 1) 'kyb)
      (write-mem! (ptr+ (get 'wptr) 2) 'scr)
      (write-mem! (ptr+ (get 'wptr) 3) 'err)
      
      
      (let loop ([iptr-prime (get 'iptr)])
        (set 'iptr (add1 (get 'iptr)))
        (let ([op (bitwise-and 
                     (read-byte iptr-prime) #x0f)]
              [fn (>> (read-byte iptr-prime) 4)]
              )
          (set 'oreg (bitwise-ior (get 'oreg) op))
          (let ([sym (lookup-instruction-symbol 'primary fn)])
            (set! INSTRUCTION-COUNT (add1 INSTRUCTION-COUNT))
 
            (debug
             (printf "~a~a~a~a~a"
                     (format "=========================")
                     (format "=========================~n")
                     (format "\t\t\t# ~a - ~a :: ~a~a~n" 
                             INSTRUCTION-COUNT
                             sym 
                             (number->string fn 16)
                             (number->string op 16)
                             )
                     (format "=========================")
                     (format "=========================~n")))
           
            
            ((lookup-instruction-thunk sym))
            
            (debug
             (show-state)
             ;;(show-memory)
             )
          ))
        
        (loop (get 'iptr)))
    )))

 

;                                                      
;                                                      
;                                                      
;   ;       ;                ;       ;                 
;   ;;     ;;                ;                         
;   ;;     ;;                ;                         
;   ; ;   ; ;  ;;;     ;;;   ; ;;    ;   ; ;;     ;;;  
;   ; ;   ; ; ;   ;   ;   ;  ;;  ;   ;   ;;  ;   ;   ; 
;   ;  ; ;  ;     ;  ;       ;   ;   ;   ;   ;  ;    ; 
;   ;  ; ;  ;  ;;;;  ;       ;   ;   ;   ;   ;  ;;;;;; 
;   ;   ;   ; ;   ;  ;       ;   ;   ;   ;   ;  ;      
;   ;   ;   ; ;   ;   ;   ;  ;   ;   ;   ;   ;   ;     
;   ;       ;  ;;;;;   ;;;   ;   ;   ;   ;   ;    ;;;; 
;                                                      
;                                                      
;                                                      



(define init
  (lambda ()

    ;;REGISTERS
    (hash-table-put! *mach* 'areg 0)
    (hash-table-put! *mach* 'breg 0)
    (hash-table-put! *mach* 'creg 0)
    (hash-table-put! *mach* 'oreg 0)
    (hash-table-put! *mach* 'tnext 0)
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
    (hash-table-put! *mach* 'iptr 0)
    ;; tptr: Pointer to the mems on the queue, 
    ;; currently timed out, waiting to run. 
    (hash-table-put! *mach* 'tptr 'NotTimerProcess.p) 
    ;; fptr, bptr: Pointers to the front and back
    ;; of the scheduling queue
    (hash-table-put! *mach* 'fptr NotProcess.p)
    (hash-table-put! *mach* 'bptr NotProcess.p)
   
    ))	 


     
;; Consumes a number of WORDS of memory to allocate.
;; This is also viewable as the number of slots in 
;; the hash table that represents the memory of our 
;; tvm.
#|
(define mem-size!
  (lambda (size)
    (set! MEM-SIZE size)))
|#

;; End word address... hence the resize 
(define mem-size!
  (lambda (size)
    (printf "Setting mem-size to ~a~n" (resize size))
    (set! MEM-SIZE (resize size))))
  

(define resize
  (lambda (n)
    (arithmetic-shift n (arithmetic-shift WORDSIZE -1))))


;                               
;                               
;                               
;   ;                           
;   ;                           
;   ;                  ;        
;   ;   ; ;;     ;;;  ;;;;  ; ; 
;   ;   ;;  ;   ;      ;    ;;  
;   ;   ;   ;   ;;     ;    ;   
;   ;   ;   ;    ;;    ;    ;   
;   ;   ;   ;      ;   ;    ;   
;   ;   ;   ;      ;   ;    ;   
;   ;   ;   ;   ;;;     ;;  ;   
;                               
;                               
;                               
(define primary 
  '(j ldlp pfix ldnl ldc ldnlp nfix ldl adc
      call cj ajw eqc stl stnl opr))

(define secondary
  '(rev lb bsub endp diff add gcall in prod 
        gt wsub out sub startp outbyte outword))

(define hash21
  '(seterr #f resetch csub0 #f stopp ladd stlb
           sthf norm ldiv ldpi stif xdble ldpri rem))

(define hash22
  '(ret lend ldtimer #f #f #f #f #f #f testerr
        testpranal tin div #f dist disc))

(define hash23
  '(diss lmul not xor bcnt lshr lshl lsum lsub 
         runp xword sb gajw savel saveh wcnt))

(define hash24
  '(shr shl mint alt altwt altend and enbt enbc 
        enbs move or csngl ccnt1 talt ldiff))

(define hash25
  '(sthb taltwt sum mul sttimer stoperr cword
         clrhalterr sethalterr testhalterr
         dup move2dinit move2dall move2dnonzero
         move2dzero #f))

(define hash26
  '(#f #f #f unpacksn #f #f #f #f #f #f #f
       #f postnormsn roundsn #f #f))

(define hash27
  '(#f ldinf fmul cflerr crcword crcbyte bitcnt
       bitrevword bitrevnbits #f #f #f #f
       #f #f #f))

(define hash28 
  '(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
(define hash29
  '(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define hash2A
  '(zero one getpri three four five six seven eight nine aye bee see dee eee eph))

;;Checks at load-time that all the above are
;; the right length.
(unless (andmap (lambda (ls)
                  (= (length ls) 16))
                (list primary secondary hash21
                      hash22 hash23 hash23 hash24
                      hash25 hash26 hash27 hash28
                      hash29 hash2A))
  (map (lambda (ls)
                  (printf "~a : ~a~n" (length ls) ls)
                  (= (length ls) 16))
                (list primary secondary hash21
                      hash22 hash23 hash23 hash24
                      hash25 hash26 hash27 hash28
                      hash29 hash2A))
  (error "Instructions tables are not all the right length."))
  
(define primary-instruction-symbol-list primary)
(define secondary-instruction-symbol-list 
  (append secondary hash21
        hash22 hash23 hash24
        hash25 hash26 hash27 hash28
        hash29 hash2A))

;                                                 
;                                                 
;                                                 
;   ;;;;;       ;                                 
;   ;    ;                                        
;   ;    ;                                        
;   ;    ; ; ;  ;   ; ;;  ;;    ;;;    ; ; ;     ;
;   ;    ; ;;   ;   ;;  ;;  ;  ;   ;   ;;  ;     ;
;   ;;;;;  ;    ;   ;   ;   ;      ;   ;    ;   ; 
;   ;      ;    ;   ;   ;   ;   ;;;;   ;    ;   ; 
;   ;      ;    ;   ;   ;   ;  ;   ;   ;     ; ;  
;   ;      ;    ;   ;   ;   ;  ;   ;   ;     ; ;  
;   ;      ;    ;   ;   ;   ;   ;;;;;  ;      ;   
;                                             ;   
;                                             ;   
;                                            ;     

(add-instruction pfix
  (set 'oreg (<< (get 'oreg) 4)))

(add-instruction nfix
  (set 'oreg (<< (word->integer 
                  (word-not (word (get 'oreg)))) 4)))

(add-instruction opr
    ;;Offset the oreg by 16, because I thunked all the instructions
  ;; down in one list... this should change, because if something
  ;; goes wrong in the primaries, you shouldn't get a secondary
  ;; by mistake...
    (let* ([sym (lookup-instruction-symbol 'secondary (get 'oreg))]
           [thunk (lookup-instruction-thunk sym)])

      ;;(show-state)
      ;;(show-memory)
      
      ;;(set! INSTRUCTION-COUNT (sub1 INSTRUCTION-COUNT))
      (debug 
       (printf "~a~a~a"
               (format "=========================~n")
               (format "\t\t~a : (count ~a)~n" sym 'XXX)
               (format "=========================~n")))
      (thunk)
      (clear 'oreg)
      ))

(add-instruction+ adc 
  (lambda (n)
    (stack! (word->integer (word+ (word a) (word n))) b c)
    (clear 'oreg)
    ))

(add-instruction+ ajw
  (lambda (n)
    (set 'wptr (ptr+ (get 'wptr) n))
    (clear 'oreg)))

(add-instruction+ call  
  (lambda (n)	    
    (let* ([old-wptr (get 'wptr)]
           [new-wptr (ptr- old-wptr 4)])
      (set 'wptr new-wptr)
      (write-mem! (ptr+ new-wptr 0) (get 'iptr))
      (write-mem! (ptr+ new-wptr 1) (get 'areg))
      (write-mem! (ptr+ new-wptr 2) (get 'breg))
      (write-mem! (ptr+ new-wptr 3) (get 'creg))
      (set 'areg (get 'iptr))
      (set 'iptr (word++ (get 'iptr) n))
      (clear 'oreg)
      )))

(define word++
  (lambda (a b)
    (word->integer 
     (word+ (word a) 
            (word b)))))

(add-instruction+ cj 
  (lambda (n)
    (if (= (get 'areg) 0)
        (set 'iptr (word++ n (get 'iptr)))
        (stack! b c undef)
        )
    (clear 'oreg)))

(add-instruction+ eqc
  (lambda (n)
    (if (equal? (get 'areg) n)
        (set 'areg 1)
        (set 'areg 0))
    (clear 'oreg)))

;;TO-DO 20040218
;; This may cause the process to be descheduled. 
(add-instruction+ j 
  (lambda (n)
    (set 'iptr (word->integer (word+ (word (get 'iptr)) (word n))))
    (stack! undef undef undef)
    (clear 'oreg)
    ))



(add-instruction+ ldc
  (lambda (n)
    (stack! (word->integer (word n)) a b)
    (clear 'oreg)))

(add-instruction+ ldl 
  (lambda (n)
    (stack! (read-mem (ptr+ (get 'wptr) n)) a b)
    (clear 'oreg)))

(add-instruction+ ldlp 
  (lambda (n)
    (stack! (ptr+ (get 'wptr) n) a b)
    (clear 'oreg)))


;;WARNING 20040215
;; The mem, technically, isn't the same as everything
;; else, regardless of how we implement it. That said, it 
;; could just be part of one big contiguous memory chunk. So,
;; for now, we seem to be (correctly or incorrectly) treating
;; the MEM vector as all the memory that exists in our
;; entire world.
(add-instruction+ ldnl 
  (lambda (n)
    (set 'areg (read-mem (ptr+ (get 'areg) n)))
    (clear 'oreg)
    ))

(add-instruction+ ldnlp 
  (lambda (n)
    (stack! (ptr+ (get 'areg) n) b c)
    (clear 'oreg)))

(add-instruction+ stl 
  (lambda (n)
    (write-mem! (ptr+ (get 'wptr) n) (get 'areg))
    (stack! b c undef)
    (clear 'oreg)))

(add-instruction+ stnl 
  (lambda (n)
    (write-mem! (ptr+ (get 'areg) n) (get 'breg))
    (stack! c undef undef)
    (clear 'oreg)))


;                                                                  
;                                                                  
;                                                                  
;    ;;;;                                    ;                     
;   ;    ;                                   ;                     
;   ;                                        ;                     
;   ;       ;;;    ;;;    ;;;    ; ;;     ;; ;   ;;;    ; ; ;     ;
;    ;;    ;   ;  ;   ;  ;   ;   ;;  ;   ;  ;;  ;   ;   ;;  ;     ;
;      ;  ;    ; ;      ;     ;  ;   ;  ;    ;      ;   ;    ;   ; 
;       ; ;;;;;; ;      ;     ;  ;   ;  ;    ;   ;;;;   ;    ;   ; 
;       ; ;      ;      ;     ;  ;   ;  ;    ;  ;   ;   ;     ; ;  
;   ;   ;  ;      ;   ;  ;   ;   ;   ;   ;  ;;  ;   ;   ;     ; ;  
;    ;;;    ;;;;   ;;;    ;;;    ;   ;    ;; ;   ;;;;;  ;      ;   
;                                                              ;   
;                                                              ;   
;               
;    

(add-instruction ldpi
  (stack! (word->integer
           (word+ (word (get 'iptr)) 
                  (word (get 'areg))))
          (get 'breg)
          (get 'creg)))

(add-instruction lend
  (let* ([breg (get 'breg)]
         [breg+1 (ptr+ (get 'breg) 1)]
         [bmem (read-mem breg)]
         [bmem+1 (read-mem breg+1)])
    ;;(printf "breg: ~a~nbreg+1: ~a~nbmem: ~a~nbmem+1: ~a~n"
    ;;        breg breg+1 bmem bmem+1)
    (cond
      [(< 1 bmem+1)
       (write-mem! breg+1 (sub1 bmem+1))
       (write-mem! breg   (add1 bmem))
       (set 'iptr (- (get 'iptr) (get 'areg)))]
            
      [else
       (write-mem! breg+1 (sub1 bmem+1))])
    (stack! a b undef)))
       

(add-instruction rev
  (stack! b a c))

;;WARNING
;; Unsigned
(add-instruction diff
  (stack! (- b a) c undef))

(add-instruction add
  (stack! (+ (get 'areg) (get 'breg)) c undef))

(add-instruction bsub
  (stack! (+ a b) c undef))

(add-instruction csub0
  (set 'error-flag (>= (get 'breg) (get 'areg)))
  (stack! b c undef))

(add-instruction ccnt1
  (if (or (zero? (get 'breg))
	  (> (get 'breg) (get 'areg)))
      (set 'error-flag #t))
  (stack! b c undef))
	   
(add-instruction div
  (if (or (= (get 'areg) 0)
	  (and (= (get 'areg) -1)
	       (= (get 'breg) MinInt)))
      (begin (stack! undef c undef)
	     (set 'error-flag #t))
      (stack! (quotient b a) c undef)))

(add-instruction dup
  (stack! a a b))
      
(add-instruction endp
  (if (= (read-mem (ptr1 (get 'areg))) 1)
      ;;THEN
      (begin
	(write-mem! (ptr1 (get 'areg)) 0)
	(set 'iptr (read-mem (get 'areg)))
	(set 'wptr (get 'areg))
	(stack! undef undef undef))
      ;;ELSE
      (begin
	(write-mem!
	 (ptr1 (get 'areg))
	 (sub1 (read-mem (ptr1 (get 'areg)))))
	(stack! undef undef undef)
	(run-next-on-queue))))
      
(add-instruction lb
  (stack! (read-byte a) b c))

(add-instruction sb
  (write-byte! (get 'areg) (get 'breg))
  (stack! c undef undef))
      
(add-instruction mul
  (stack! (* (get 'areg) (get 'breg)) c undef))
      
;; COMMENT 20040218
;; (startp) became the magic 'add-to-queue' macro. Woo.
(add-instruction startp
  (add-to-queue 'areg (+ (get 'breg) (get 'iptr))))

(add-instruction gt
  (if (> (get 'breg) (get 'areg))
      (stack! 1 c undef)
      (stack! 0 c undef)))

;; BIG WARNING 20040224
;; Don't know if this should be MinInt (the number) or 'MinInt (the symbol)
(add-instruction mint
  (stack! MinInt a b))

(add-instruction rem
  ;;Unless (= Areg 0) or (and (= Areg -1) (= Breg MinInt))
  (if (or (= (get 'areg) 0)
	  (and (= (get 'areg) -1) (= (get 'breg) MinInt)))
      ;;Oops!
      (begin
	(stack! undef c undef)
	(set 'error-flag #t))
      (let ([result (remainder (get 'breg) (get 'areg))]) 
	(stack! result c undef))))

(add-instruction ret
  (set 'iptr (read-mem (get 'wptr)))
  (set 'wptr (ptr+ (get 'wptr) 4))
  (if (equal? (get 'iptr) 'finished)
      (begin
        (show-state)
        (show-memory)
        (error "We're all done."))))

;;error flag if arith overflow occurs
(add-instruction sub
  (stack! (- b a) c undef))
      
(add-instruction wsub
  (stack! (ptr+ a b) c undef))
      
(add-instruction null
  (stack! 'NULL a b))


;                                                               
;                                                               
;                                                               
;    ;;;;         ;                  ;           ;              
;   ;    ;        ;                  ;           ;              
;   ;             ;                  ;           ;              
;   ;       ;;;   ; ;;     ;;;    ;; ;   ;   ;   ;    ;;;   ; ; 
;    ;;    ;   ;  ;;  ;   ;   ;  ;  ;;   ;   ;   ;   ;   ;  ;;  
;      ;  ;       ;   ;  ;    ; ;    ;   ;   ;   ;  ;    ;  ;   
;       ; ;       ;   ;  ;;;;;; ;    ;   ;   ;   ;  ;;;;;;  ;   
;       ; ;       ;   ;  ;      ;    ;   ;   ;   ;  ;       ;   
;   ;   ;  ;   ;  ;   ;   ;      ;  ;;   ;  ;;   ;   ;      ;   
;    ;;;    ;;;   ;   ;    ;;;;   ;; ;    ;; ;   ;    ;;;;  ;   
;                                                               
;                                                               
;                                                               


(define run-next-on-queue
  (let ()
    
    (define checker
      (lambda ()
        (let ([front (get 'fptr)]
              [back  (get 'bptr)]
              [timer (get 'tptr)])
          ;;If the timer queue isn't empty, we ain't deadlocked.
          (cond
            [(and (equal? front back)
                  (equal? front NotProcess.p)
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

             (debug 
              (printf " *1* Something in the queue is ready to run.~n")
              (printf "iptr  : ~a~n" (get 'iptr))
              (printf "wptr  : ~a~n" (get 'wptr))
              (printf "tptr  : ~a~n" (get 'tptr))
              (printf "tnext : ~a~n" (get 'tnext)))
             
             ]
            
            ;; We're not deadlocked, there's nothing on the timer 
            ;; queue, so it seems like we're free to run things
            ;; off the regular queue. 
            
            [(not (equal? front NotProcess.p))
             (set 'wptr front)
             (set 'iptr (workspace wptr iptr))
             
             (debug 
              (printf " *2* Nothing on timer queue; run from reg. queue.~n")
              (printf "wptr  : ~a~n" (get 'wptr))
              (printf "iptr  : ~a~n" (get 'iptr)))
             ;; We're either last on the queue in this case, or 
             ;; somewhere in the queue. 
             (cond
               [(equal? front back)
                ;; last on queue.
                (set 'fptr NotProcess.p)
                (set 'bptr NotProcess.p)
                (debug
                 (printf " *2a* Last on the queue.~n")
                 (printf "fptr  : ~a~n" (get 'fptr))
                 (printf "bptr  : ~a~n" (get 'bptr)))]
               [else
                ;; otherwise, point to the next process
                (set 'fptr (workspace front next-ws))
                (debug 
                 (printf " *2b* Not last on queue; point to next.~n")
                 (printf "fptr  : ~a~n" (get 'fptr)))
                ])]
            
            [(not (equal? timer 'NotTimerProcess.p))
             ;;This is a busywait case. 
             (debug 
              (printf "Busywaiting for ~a~n" sleep-duration))
             (sleep sleep-duration)
             (checker)]
            
            [else
             ;;Many kinds of badness. We fuckethed this up.
             (error "No. Bad. Process badness. You are so humped.")]))))
    
    ;;Looking for phantom ALT processes; see instruction DIST
    ;; in "Inside the Transputer", page 118 for more information. 
    ;; We are removing this process from the queue if Disabling.p
    ;; is set in (Wptr - 3) or ALT-STATE. Then, we'll run the scheduler
    ;; as normal.
    
    ;; matt thinks (get 'tptr) should be wptr, but can't defend why.
    
    ;;(printf "w ~a t ~a~n" (get 'wptr) (get 'tptr))
    (lambda ()
      (unless (equal? 'NotTimerProcess.p (get 'tptr))
	;;(printf "w ~a t ~a~n" (get 'wptr) (get 'tptr))
	(let loop ([this-alt-state (workspace (get 'tptr) alt-state)])
	  (if (equal? this-alt-state Disabling.p)
	      (begin
		(printf "--- HUMPED ---~n")
		(set 'tptr (workspace (get 'tptr) next-t))
		(set 'tnext (workspace (get 'tptr) timeout))
		(loop (workspace (get 'tptr) alt-state))
		))))
      (checker)
      )))


;                                                     
;                                                     
;                                                     
;     ;;;;   ;                                     ;  
;    ;    ;  ;                                     ;  
;   ;        ;                                     ;  
;   ;        ; ;;    ;;;    ; ;;    ; ;;     ;;;   ;  
;   ;        ;;  ;  ;   ;   ;;  ;   ;;  ;   ;   ;  ;  
;   ;        ;   ;      ;   ;   ;   ;   ;  ;    ;  ;  
;   ;        ;   ;   ;;;;   ;   ;   ;   ;  ;;;;;;  ;  
;   ;        ;   ;  ;   ;   ;   ;   ;   ;  ;       ;  
;    ;    ;  ;   ;  ;   ;   ;   ;   ;   ;   ;      ;  
;     ;;;;   ;   ;   ;;;;;  ;   ;   ;   ;    ;;;;  ;  
;                                                     
;                                                     
;                                                     

;; WARNING 20040218
;; Both (in) and (out) are interruptable. This
;; will likely never be implemented in our interpreter.
;; Not for a while, anyway. Christian claims it's easy. 
;; I doubt it. I suspect tedium.
(add-instruction in
  (let ([num-of-bytes   (get 'areg)]
	[channel        (get 'breg)]
	[write-start    (get 'creg)])
    (cond
     [(equal? (read-mem channel) NotProcess.p)
      
      (debug (printf "=== IN : NOTPROCESS.P ===~n"))
     
      (write-mem! channel (get 'wptr))
      (workspace! wptr channel write-start)
      ;; Storing the iptr is taken care of by 'add-to-queue'
      ;; Nope. But, we might change our minds again.
      (workspace! wptr iptr (get 'iptr))
      
      (debug
       (printf "\tworkspace:wptr ~a~n"
               (workspace wptr channel))
       (printf "\tworkspace:iptr ~a~n"
               (workspace wptr iptr))
       (printf "\tmem ~a (~a)~n" channel (read-mem channel))
       
       (printf "<BEFORE run-next-on-queue>~n")
       (show-state)
       (show-memory)
       (printf "</BEFORE run-next-on-queue>~n")
       )
       
      (run-next-on-queue) ]
     
     ;;Otherwise, we can read some data.
     [else
      (debug 
       (printf "=== IN : READING DATA~n"))
      
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
      (write-mem! channel NotProcess.p) ])
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
  (let ([count     (get 'areg)]
	[chan      (read-mem (get 'breg))]
	[source    (get 'creg)])

    (debug
     (printf " *4* OUT~n")
     (printf "count  : ~a~n" count)
     (printf "chan   : ~a~n" chan)
     (printf "source : ~a~n" source))
    
    (cond
     [(member chan '(scr err))
      (debug
       (printf " *4a* Dealing with SCR or ERR~n"))
      (error "Not fixed yet.")
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
	  
     [(equal? chan NotProcess.p)
      (debug (printf " *4b* -- NOTPROCESS.P --~n"))
      ;;(error "Not fixed yet.")
      ;; If 'chan' is NotProcess, then we obviously can't write
      ;; to the location pointed to by this address; we need
      ;; to actually put something in memory at breg.
      (write-mem! (get 'breg) (get 'wptr))
      (workspace! wptr iptr (get 'iptr))
      (workspace! wptr channel source)
      (run-next-on-queue) ]
     
     [(and 
       (not (equal? chan NotProcess.p))
       (equal? (workspace chan alt-state) Enabling.p))
      (debug (printf " *4c* -- NOTPROCESS.P && ENABLING.P --~n"))
      (error "Not fixed yet.")
      (workspace! (read-mem chan-ptr) alt-state Disabling.p)
      (workspace! chan-ptr top (get 'wptr))
      (workspace! wptr iptr (get 'iptr))
      (workspace! wptr alt-state read-start)
      (run-next-on-queue) ]

     [(and (not (equal? chan NotProcess.p))
	   (equal? (workspace chan alt-state) Waiting.p))
      (debug (printf " *4d* -- NOTPROCESS.P && WAITING.P --~n"))
      ;;Need to swap before we obliterate things in memory.
      (workspace! chan alt-state Disabling.p)
      (workspace! chan top (get 'wptr))
      (workspace! wptr iptr (get 'iptr))
      (workspace! wptr alt-state source)
      
      ;;The remaining difference: according to the white book,
      ;; ALT process is rescheduled, and this process
      ;; is descheduled.
      ;;
      ;;I do not trust the code below to be correct; or,
      ;; I do not know if it is correct.
      (add-to-queue 'wptr (get 'iptr))
      (set 'wptr chan)
      (set 'iptr (workspace wptr iptr))
      (run-next-on-queue)

      
      
      #|
      (let ([old-chan-word (read-mem chan-ptr)])
        (debug 
         (printf "Before the swap...~n")
         (show-state)
         (printf "mem(~a) == ~a~n"
                 (read-mem chan-ptr)
                 (workspace (read-mem chan-ptr) alt-state))
         (printf "mem(~a) == ~a~n"
                 chan-ptr
                 (workspace chan-ptr top))
         (printf "mem(~a) == ~a~n"
                 (ptr- (get 'wptr) 1)
                 (workspace wptr iptr))
         (printf "mem(~a) == ~a~n"
                 (ptr- (get 'wptr) 3)
                 (workspace wptr channel)))
         
	(workspace! (read-mem chan-ptr) alt-state Disabling.p)
	(workspace! (read-mem chan-ptr) top (get 'wptr))
	(workspace! wptr iptr (get 'iptr))
	(workspace! wptr channel read-start) 
	(add-to-queue 'wptr (get 'iptr))
        (set 'wptr old-chan-word)
	(set 'iptr (workspace wptr iptr))
        
        (debug 
         (printf "~nAfter the swap...~n")
         (show-state)
         (printf "mem(~a) == ~a~n"
                 (read-mem old-chan-word)
                 (workspace (read-mem old-chan-word) alt-state))
         (printf "mem(~a) == ~a~n"
                 chan-ptr
                 (workspace chan-ptr top))
         (printf "mem(~a) == ~a~n"
                 (ptr- (get 'wptr) 1)
                 (workspace wptr iptr))
         (printf "mem(~a) == ~a~n"
                 (ptr- (get 'wptr) 3)
                 (workspace wptr channel)))
	)|#
      ]

     [(and (not (equal? chan NotProcess.p))
		(equal? (workspace chan alt-state) Disabling.p))
      (debug (printf "-- NOTPROCESS.P && DISABLING.P --~n"))
      ;;(error "Not fixed yet.")
      (workspace! chan top (get 'wptr))
      (workspace! wptr iptr (get 'iptr))
      (workspace! wptr channel source)
      (run-next-on-queue)]

     [else
      ;;WARNING 20040218
      ;; There are two differences here between (in) and (out).
      ;; The most important is the memory location from which we read
      ;; or write is different in each case. In (out), it is 
      ;; (- (read-mem chan-ptr) 3), whereas (in) just reads from 
      ;; channel (or the Breg).
      (debug (printf "-- ELSE --~n"))
      (error "Not fixed yet.")
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
      (write-mem! chan-ptr NotProcess.p) 
      ] 
     ;;end else
     ))
       
  (stack! undef undef undef))


(add-instruction outbyte
  ;;(write-mem! (get 'wptr) (get 'areg))
  (workspace! wptr top (get 'areg))
  (stack! 1 b (get 'wptr))
  ((lookup-instruction-thunk 'out)))
 
(add-instruction outword
  (write-mem! (get 'wptr) (get 'areg))
  (stack! WORDSIZE b (get 'wptr))
  ((lookup-instruction-thunk 'out)))
  ;;(ChannelInstr '(out)))


;                    
;                    
;                    
;      ;     ;       
;      ;     ;       
;     ; ;    ;   ;   
;     ; ;    ;  ;;;; 
;    ;   ;   ;   ;   
;    ;   ;   ;   ;   
;   ;;;;;;;  ;   ;   
;   ;     ;  ;   ;   
;   ;     ;  ;   ;   
;  ;       ; ;    ;; 
;                    
;                    
;                    

(add-instruction alt
  (workspace! wptr alt-state Enabling.p))


(add-instruction altend
  (set 'iptr (word++ (get 'iptr) (workspace wptr top))))

      
(add-instruction altwt
  (let ()
    (cond
      [(equal? (workspace wptr alt-state)  Disabling.p)
       (workspace! wptr top NoneSelected.o)
       (debug
        (printf " *3a* wptr:alt-state is disabling.p.~n")
        (printf " Setting top of current workspace to ~a~n" 
                NoneSelected.o))
       ]
      [else
       (workspace! wptr alt-state Waiting.p)
       (workspace! wptr iptr (get 'iptr))
       (workspace! wptr top NoneSelected.o)
       (debug
        (printf " *3b* wptr:alt-state is NOT disabling.p.~n")
        (printf "In the current workspace, setting:~n")
        (printf " alt-state : ~a~n" Waiting.p)
        (printf " iptr      : ~a~n" (get 'iptr))
        (printf " top       : ~a~n" NoneSelected.o))
       ])
    (run-next-on-queue)
    (stack! undef undef undef)))


(add-instruction disc
  (let* ([breg (get 'breg)]
	 [w-deref (workspace wptr top)]
	 [creg-deref (read-mem (get 'creg))]
	 [fired? (and (not (= breg 0))
		      (equal? w-deref NoneSelected.o)
		      (not (equal? creg-deref NotProcess.p))
		      (not (= creg-deref (get 'wptr)))
		      )])
    (if fired? (workspace! wptr top (get 'areg)))
    
    ;;Additionally
    ;; If this test is true, then it implies we are looking
    ;; at ourselves. So, we should destroy evidence of 
    ;; a waiting process (US!) before doing everything else.
    (if (and (not (zero? breg))
	     (= creg-deref (get 'wptr)))
	(write-mem! (get 'creg) NotProcess.p))
	 
    (stack! (if fired? 1 0) undef undef)))
      
(add-instruction diss
  (let ([fired? (and (not (zero? (get 'breg)))
		     (equal? (workspace wptr top) NoneSelected.o))])
    
    (if fired? (workspace! wptr top (get 'areg)))
    (stack! (if fired? 1 0) c undef)))
      
(add-instruction dist
  (let* ([current-time (current-milliseconds)]
	 [fired? (and
		  (and (not (zero? (get 'breg)))
		       (equal? (workspace wptr top) NoneSelected.o))
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
       [(equal? (read-mem (get 'breg)) NotProcess.p)
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
	  (workspace! wptr alt-state Disabling.p)]
	 )]))
  
  ;;Oops! Don't forget, we have to ... change the stack.
  (stack! a c undef))


(add-instruction enbs
  ;;The stack is unaffected by this instruction
  (if (= (get 'areg) 1)
      (workspace! wptr alt-state Disabling.p)))


(add-instruction enbt
  (cond
   [(= (get 'areg) 0)
    ;;Do nothing
    (stack! a c undef)
    (void)]
   [else
    (cond
     [(equal? (workspace wptr alt-t) TimeNotSet.p)
      (workspace! wptr alt-t TimeSet.p)
      (workspace! wptr timeout (get 'breg))]
     [(AFTER? (workspace wptr timeout) (get 'breg))
      (workspace! wptr timeout (get 'breg))])])
  (stack! a c undef))

      
(add-instruction talt
  (workspace! wptr alt-state Enabling.p)
  (workspace! wptr alt-t TimeNotSet.p))

(add-instruction taltwt
  (let ([current-time (current-milliseconds)])
	 (cond
	  [(or 
	    (equal? (workspace wptr alt-state) ;;Enabling.p)
		    Disabling.p)
	    (and (equal? (workspace wptr alt-t) TimeSet.p)
		 (BEFORE? (workspace wptr timeout) current-time)))
	   ;;Undef the stack, blast the top of the workspace
	   
	   
	   
	   (stack! undef undef undef)
	   (workspace! wptr top NoneSelected.o)
	   ;; And do nothing.
	   (void)]

	  
	  [(equal? (workspace wptr next-t) TimeNotSet.p)
	   (workspace! wptr alt-state Waiting.p)
	   (stack! undef undef undef)
	   (run-next-on-queue)]


	  [else
	   
	   ;;Insert ourselves
	   ;;(printf "~n\tHere~n")
	   (traverse-and-insert (get 'tptr) (get 'tptr))
	   (workspace! wptr alt-state Waiting.p)
	   (stack! undef undef undef)
	   (workspace! wptr top NoneSelected.o)
;;							(show-state)
;;							(error)
	   (run-next-on-queue)
	   ])))


;                                      
;                                      
;                                      
;  ;;;;;;;  ;                          
;     ;                                
;     ;                                
;     ;     ;   ; ;;  ;;     ;;;   ; ; 
;     ;     ;   ;;  ;;  ;   ;   ;  ;;  
;     ;     ;   ;   ;   ;  ;    ;  ;   
;     ;     ;   ;   ;   ;  ;;;;;;  ;   
;     ;     ;   ;   ;   ;  ;       ;   
;     ;     ;   ;   ;   ;   ;      ;   
;     ;     ;   ;   ;   ;    ;;;;  ;   
;                                      
;                                      
;                                      

;;TWO INSTRUCTIONS

(add-instruction ldtimer
  ;;Comes back as a fixnum (30 bits w/ 2 for sign)
  (stack! (current-milliseconds) a b))
      
(add-instruction tin
  ;;This is interruptable on the transputer. We don't have this. Yet.
  ;; Ever. Possibly. Probably.
  (let ([current-time (current-milliseconds)]
	[reschedule-time (get 'areg)])

    ;;If our reschedule is in the past, we've already timed out
    (if (AFTER? current-time reschedule-time)
	;;We go on. Don't do stuff now.
	(void)
	;;Otherwise, we need to do stuff.
	(begin
	  ;;we need to insert ourselves in the proper place.
	  ;; This is an ordered queue.
	  ;;Store our reschedule time 
	  (workspace! wptr timeout reschedule-time)
	  ;;Store the iptr
	  (workspace! wptr iptr (get 'iptr))
	  
	  (traverse-and-insert (get 'tptr) (get 'tptr))
	  (run-next-on-queue) ))
    (stack! undef undef undef)))



;                             
;                             
;                             
;   ;;;;;                  ;  
;   ;                      ;  
;   ;                      ;  
;   ;      ; ;   ;;;    ;; ;  
;   ;;;;   ;;   ;   ;  ;  ;;  
;   ;      ;   ;    ; ;    ;  
;   ;      ;   ;;;;;; ;    ;  
;   ;      ;   ;      ;    ;  
;   ;      ;    ;      ;  ;;  
;   ;      ;     ;;;;   ;; ;  
;                             
;                             
;                             

(add-instruction getpri
  (stack! 0 a b))

(add-instruction .boolinvert
  (if (= (get 'areg) 0)
      (stack! 1 b c)
      (stack! 0 b c)))

(add-instruction+ .loadlabaddr 
  (lambda (label)
    (stack! (ptr1 (get-label-location label)) a b)))
	  
(add-instruction+ .loadlabdiff 
  (lambda (l2 l1)
    (stack! (- (get-label-location l2)
	       (get-label-location l1)) a b)))

(add-instruction+ .lend 
  (lambda (number lend-label lstart-label)
    (set 'areg (- (get 'iptr)
		  (get-label-location lstart-label)))
    (set 'breg (ptr+ (get 'wptr) number))

    (if (> (read-mem (ptr1 (get 'breg))) 1)
	(begin
	  
	  ;; WARNING DANGER 20040220
	  ;; .MAGIC LOOPVAR REMOVED
	  ;; It looks like occ21 doesn't bother
	  ;; putting in an actual, honest-to-god loop variable
	  ;; when it is an "anonymous" loop. So, we're going to have
	  ;; to initialize the index to zero in that case.
	  ;; 
	  ;; Hence, this if. 
	  (if (void? (read-mem (get 'breg)))
	      (write-mem! (get 'breg) 0))
		 
	  (write-mem! (ptr1 (get 'breg)) (sub1 
					  (read-mem
					   (ptr1 (get 'breg)))))
	  (write-mem! (get 'breg) 
		      (add1 
		       (read-mem 
			(get 'breg))))
	  ;; Areg currently holds the calculated offset to the start
	  ;; of the loop; we subtract that from the instruction pointer
	  ;; to go back and run the loop again.
	  (set 'iptr (- (get 'iptr) (get 'areg))))
	(begin
	  (write-mem! (ptr1 (get 'breg)) (sub1 
					  (read-mem
					   (ptr1 (get 'breg))))))
	)))


(add-instruction+ .lendb
  (lambda (number end-label start-label)
    (error ".lendb unimplemented.")))

(add-instruction .notprocess
  (stack! NotProcess.p a b))



;                                                         
;                                                         
;                                                         
;   ;;;;;;                   ;       ;                    
;   ;     ;                  ;;     ;;                    
;   ;      ;                 ;;     ;;                    
;   ;      ; ;     ; ; ;;    ; ;   ; ;   ;;;   ; ;;  ;;   
;   ;      ; ;     ; ;;  ;   ; ;   ; ;  ;   ;  ;;  ;;  ;  
;   ;      ;  ;   ;  ;   ;   ;  ; ;  ; ;    ;  ;   ;   ;  
;   ;      ;  ;   ;  ;   ;   ;  ; ;  ; ;;;;;;  ;   ;   ;  
;   ;      ;   ; ;   ;   ;   ;   ;   ; ;       ;   ;   ;  
;   ;     ;    ; ;   ;   ;   ;   ;   ;  ;      ;   ;   ;  
;   ;;;;;;      ;    ;   ;   ;       ;   ;;;;  ;   ;   ;  
;               ;                                         
;               ;                                         
;              ;                                          

;;(load "machine.scm")
;;(load "helpers.scm")

;; zero indexed!
(define *MAX-POOLS* 31)
(define *POOL-SIZES* (make-hash-table))
(define pools (make-hash-table))
(define *DYNMEM-START* -2000)
(define *DYNMIN* *DYNMEM-START*)
(define top *DYNMEM-START*)

(define debug-mem
  (lambda (str . val)
    (fprintf 
     (current-error-port)
     (apply format 
            (append 
             (list str) val)))))
     
(define initialize-dynmem
  (lambda ()
    (hash-table-for-each 
     *POOL-SIZES* 
     (lambda (k v) 
       (hash-table-put! pools k 'PoolNull)))
    (set! top (ptr- *DYNMIN* 1))
    ))


(define make-pools
  (let ([hash (make-hash-table)])
    ;;Multiples of two, plus four.
    ;; eg. 4, 6, 8, 10...
    (define pool-size
      (lambda (n)
        (+ (* WORDSIZE 2) (* n WORDSIZE))))
    (define make-pool*
      (lambda (n)
        (let loop ([n n])
          (hash-table-put! hash n (pool-size n))
          (if (> n 0)
              (loop (sub1 n)))
          hash)))    
    (lambda (n)
      (make-pool* n))
    ))


;;;;; Important initialization stuff
(set! *POOL-SIZES* (make-pools *MAX-POOLS*))
(initialize-dynmem)



(define allocate-dynmem
  (lambda (index length)
    (let ([address (hash-table-get pools index 
                                   (lambda () #f))])
      ;;(debug-mem "Addr: ~a~n" address)
      (if (not (equal? address 'PoolNull))
          (begin
            (hash-table-put! 
             pools index (read-mem address)))
          (begin
            (set! address (ptr+ top 1))
            (set! top (+ top length))
            ;;ASSERT top <= max
            ))
      address
      )))

(define release-dynmem
  (lambda (index address)
    ;;(debug-mem "I: ~a~nA: ~a~n" index address) 
    (write-mem! address 
                (hash-table-get
                 pools index 
                 (lambda () (error "change this to something better"))))
    (hash-table-put! pools index address)))


(define malloc
  (let ()
    (define size-to-pool
      (lambda (size)
        (call/cc 
         (lambda (exit)
           (let loop ([index 0])
             (let ([psize (hash-table-get
                           *POOL-SIZES* index 
                           (lambda () 
                             (error (format "No size at index ~a." index))))])
               (if (< size psize)
                   (exit (cons index psize))
                   (if (<= index *MAX-POOLS*)
                       (loop (add1 index))))))))))
    (lambda (size)
      (let* ([size (ptr+ size 1)]
             [pair (size-to-pool size)]
             [pool-index (car pair)]
             [pool-size (cdr pair)]
             [address (allocate-dynmem pool-index pool-size)])
        ;;(debug-mem "Malloc: Sz: ~a, Pidx : ~a\t PSz: ~a~n"
        ;;size pool-index pool-size)
        (write-mem! address pool-index)
        (ptr+ address 1)))))
  
(define mrelease
  (lambda (address)
    (let* ([address (ptr- address 1)]
           [pool-index (read-mem address)])
      ;;(debug-mem "Addr: ~a~nPool Idx: ~a~n" address pool-index)
      (release-dynmem pool-index address))))


(add-instruction malloc
  (stack! (malloc (get 'areg)) b c))

(add-instruction mrelease
  (mrelease (get 'areg))
  (stack! b c undef))

(add-instruction mnew
  (stack! (allocate-dynmem (get 'areg)) b c))

(add-instruction mfree
  (release-dynmem (get 'areg) (get 'breg))
  (stack! c undef undef))
  

;                                                  
;                                                  
;                                                  
;   ;      ;         ;                             
;   ;      ;         ;                             
;   ;      ;         ;                             
;   ;      ;   ;;;   ;   ; ;;     ;;;   ; ;   ;;;  
;   ;;;;;;;;  ;   ;  ;   ;;  ;   ;   ;  ;;   ;     
;   ;      ; ;    ;  ;   ;    ; ;    ;  ;    ;;    
;   ;      ; ;;;;;;  ;   ;    ; ;;;;;;  ;     ;;   
;   ;      ; ;       ;   ;    ; ;       ;       ;  
;   ;      ;  ;      ;   ;;  ;   ;      ;       ;  
;   ;      ;   ;;;;  ;   ; ;;     ;;;;  ;    ;;;   
;                        ;                         
;                        ;                         
;           



(define clear
  (lambda (reg)
    (set reg 0)))

(define *MININT* (void))
  (define *MAXWORD* (void))

  
(define set-wordsize-constants
  (lambda ()
    (case WORDSIZE
      [(4) 
       (set! *MININT* #x80000000)
       (set! *MAXWORD* #xFFFFFFFF)
       ]
      [(2) 
       (set! *MININT* #x8000)
       (set! *MAXWORD* #xFFFF)]
      [(1)
       (set! *MININT* #x80)
       (set! *MAXWORD* #xFF)]
      )))
  ;; Left?
(define >> (lambda (n m) (arithmetic-shift n (* -1 m))))
;; Right?
(define << (lambda (n m) (bitwise-and (arithmetic-shift n m) *MAXWORD*)))


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

(define show-memory
  (lambda ()
    (let ([mem-loc (list:quicksort (hash-table-map
                               MEM 
                               (lambda (k v) k))
                              <)])
      (for-each 
       (lambda (loc)
         (let* ([w (hash-table-get MEM loc (lambda () "x"))]
                [bytes (if (number? w)
                           (word->bytes (word w))
                           w)])
         (printf "\t(~a)\t: ~a~n"
                 loc 
                 (if (list? bytes)
                     (map (lambda (i)
                            (number->string i 16))
                          bytes)
                     bytes))))
       mem-loc))))


(define ->hex
  (lambda (v)
    (cond
      [(number? v) (number->string v 16)]
      [else v])))

(define show-state
  (lambda ()
    (printf "Registers: ~n")
    (let ([regs 
	   (list:quicksort (hash-table-map 
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
	   (printf "\t~a <- ~a~n" k (->hex val))))
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





(define load-instructions
  (lambda (ls start)
    ;; Don't know if we have this right
    (let ([base start]
          [offset 0])
      (for-each 
       (lambda (inst)
         (write-byte! base inst)
         (set! base (add1 base)))
       ls))))


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
      (if (equal? (get 'fptr) NotProcess.p)
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
	    (traverse-and-insert next-wptr a-wptr)]
	   ))] ;;end else
       );;end outer cond
      )))

(parse-command-line)
