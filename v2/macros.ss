(module macros mzscheme
  (require (lib "defmacro.ss"))
  (provide (all-defined))
;                                               
;                                               
;                                               
;   ;       ;                                   
;   ;;     ;;                                   
;   ;;     ;;                                   
;   ; ;   ; ;  ;;;     ;;;   ; ;   ;;;     ;;;  
;   ; ;   ; ; ;   ;   ;   ;  ;;   ;   ;   ;     
;   ;  ; ;  ;     ;  ;       ;   ;     ;  ;;    
;   ;  ; ;  ;  ;;;;  ;       ;   ;     ;   ;;   
;   ;   ;   ; ;   ;  ;       ;   ;     ;     ;  
;   ;   ;   ; ;   ;   ;   ;  ;    ;   ;      ;  
;   ;       ;  ;;;;;   ;;;   ;     ;;;    ;;;   
;                                               
;                                               
;                                               

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


  (define-syntax debug
    (lambda (stx)
      (syntax-case stx ()
        [(_ bodies ...)
         (if #t
             #`(begin
                 bodies ...)
             #'(void))])))


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
	    (quote key) 
            (lambda () (proc (get 'oreg))))])))

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
  )