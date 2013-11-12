(module convert2 mzscheme
  
  (require (lib "pregexp.ss")
	   (lib "string.ss")
	   (lib "list.ss"))
  
  (provide convert known-instructions)


(define strip-crlf
  (lambda (str)
    (pregexp-replace* "\\r" (pregexp-replace* "\\n" str "") "")))

  
(define slurp
  (lambda (fname)
    (let ([ip (open-input-file fname)]
	  [lines '()])
      (let loop ([line (read-line ip)])
	(unless (eof-object? line)
	  (set! lines (cons (strip-crlf line) lines))
	  (loop (read-line ip))))
      (reverse lines))))

(define *known-instructions* '())
(define *ignored* '(.global .setvs .setms .filename 
			    .line .occcomment .align .end .tcoff 
			    .tsdepth .stubname
			    ))
(define START-LABEL 'foo)
(define WORKSPACE-SIZE -1)

(define known-instructions
  (lambda *instr*
    (set! *known-instructions* 
	  (map symbol->string
	       (apply append *instr*)))))
  
(define remove-comments
  (lambda (str)
    (cond 
     [(pregexp-match "(.*?)[[:blank:]]*--" str) =>
      (lambda (match)
	(cadr match))]
     [else
      str])))

(define remove-tabs
  (lambda (str)
    (pregexp-replace "^[[:blank:]]*" str "")))

(define string->any
  (lambda (str)
    (cond
     [(number? str) str]
     [(string->number str) => (lambda (x) x)]
     [else (string->symbol str)])))
  
(define etc->list
  (lambda (str)
    (string-lowercase! str)
    ;;(printf "etc->list : ~a~n" str)
    (cond
     [(pregexp-match "[[:blank:]]*([[:alpha:]]+[[:digit:]]*):[[:blank:]]*$" str) =>
      (lambda (match)
	`(label: ,(string->symbol (cadr match))))]
     [(ormap 
       (lambda (inst)
	 (let ([res (pregexp-match (string-append "^" (symbol->string inst)) str)])
	   ;;(printf "~a:~a : ~a~n" inst str res)
	   res))
       *ignored*) #f]
      
     
     [else
      (map
       string->any
       (map (lambda (str)
	      (pregexp-replace "[[:blank:]]*" str ""))
	    (pregexp-split " " str)))]
     )))

(define get-nifty-info
  (lambda (ls)
    (cond
     [(null? ls) '()]
     [else
      (let ([inst (car ls)]
	    [head (caar ls)])
	(cond

	 [(equal? '.jentry head) 
	  (set! START-LABEL 
		(if (symbol? (cadr inst))
		    (cadr inst)
		    (string->symbol (cadr inst))))
	  (get-nifty-info (cdr ls))]
	 [(equal? '.setws head)  (let ([val 
					(+  4 (string->any (cadr inst)))])
				   (if (> val WORKSPACE-SIZE)
				       (set! WORKSPACE-SIZE val)))
	  (get-nifty-info (cdr ls))]
	 [else
	  (cons (car ls) (get-nifty-info (cdr ls)))]))
      ])))

(define convert
  (lambda (fname . wspace-lab)
    (let ([code
	    (filter (lambda (ls)
		      (and (list? ls) (not (null? ls))))
		    (map etc->list 
			 (map remove-tabs
			      (filter (lambda (str)
					;;(printf "Checking ~a~n" str)
					(and (string? str)
					     (> (string-length str) 0)))
				      (map remove-comments
					   (slurp fname))))))])
      (if (not (null? wspace-lab))
	  (begin
	    (set! WORKSPACE-SIZE (car wspace-lab))
	    (set! START-LABEL (cadr wspace-lab)))
	  (set! code (get-nifty-info code)))

      (append
       `(
	 )
       code
       `((label: entry-point)
	 (call ,START-LABEL)
	 (mem ,WORKSPACE-SIZE)
	 (init)
	 (run entry-point)
	 )))))

)
      
