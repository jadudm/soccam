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
  (stack! 'NotProcess.p a b))

