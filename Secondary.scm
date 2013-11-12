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
  (set 'wptr (ptr+ (get 'wptr) 4)))

;;error flag if arith overflow occurs
(add-instruction sub
  (stack! (- b a) c undef))
      
(add-instruction wsub
  (stack! (ptr+ a b) c undef))
      
(add-instruction null
  (stack! 'NULL a b))