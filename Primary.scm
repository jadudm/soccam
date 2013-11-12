(add-instruction+ adc 
  (lambda (n)
    (stack! (+ a n) b c)))

(add-instruction+ ajw
  (lambda (n)
    (set 'wptr (+ (resize n) (get 'wptr)))))

(add-instruction+ call 
  (lambda (label)	    
    (let* ([old-wptr (get 'wptr)]
	   [new-wptr (ptr- old-wptr 4)])
      (set 'wptr new-wptr)
      (write-mem! (ptr+ new-wptr 0) (get 'iptr))
      (write-mem! (ptr+ new-wptr 1) (get 'areg))
      (write-mem! (ptr+ new-wptr 2) (get 'breg))
      (write-mem! (ptr+ new-wptr 3) (get 'creg))
      (set 'areg (get 'iptr))
      (set 'iptr (get-label-location label))
      )))

(add-instruction+ cj 
  (lambda (label)
    (if (= (get 'areg) 0)
	(set 'iptr (get-label-location label))
	(stack! b c undef)
	)))

(add-instruction+ eqc
  (lambda (n)
    (if (equal? (get 'areg) n)
	(set 'areg 1)
	(set 'areg 0))))

;;TO-DO 20040218
;; This may cause the process to be descheduled. 
(add-instruction+ j 
  (lambda (label)
    (set 'iptr (get-label-location label))
    (stack! undef undef undef)))

	   

(add-instruction+ ldc
  (lambda (n)
    (stack! n a b)))

(add-instruction+ ldl 
  (lambda (n)
    (stack! (read-mem (ptr+ (get 'wptr) n)) a b)))

(add-instruction+ ldlp 
  (lambda (n)
    (stack! (ptr+ (get 'wptr) n) a b)))


;;WARNING 20040215
;; The mem, technically, isn't the same as everything
;; else, regardless of how we implement it. That said, it 
;; could just be part of one big contiguous memory chunk. So,
;; for now, we seem to be (correctly or incorrectly) treating
;; the MEM vector as all the memory that exists in our
;; entire world.
(add-instruction+ ldnl 
  (lambda (n)
    (set 'areg (read-mem (ptr+ (get 'areg) n)))))

(add-instruction+ ldnlp 
  (lambda (n)
    (stack! (ptr+ (get 'areg) n) b c)))

(add-instruction+ stl 
  (lambda (n)
    (write-mem! (ptr+ (get 'wptr) n) (get 'areg))
    (stack! b c undef)))

(add-instruction+ stnl 
  (lambda (n)
    (write-mem! (ptr+ (get 'areg) n) (get 'breg))
    (stack! c undef undef)))
