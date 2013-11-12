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
  
(define test1
  (lambda ()
    (begin 
    (initialize-dynmem)
    (mem-size! 300)
    (let ([one (malloc 39)]
          [two (malloc 39)])
      ;;(printf "~a ~a~n" one two)
      (mrelease one)
      (printf "Whee!~n")
      (let ([three (malloc 39)])
        three)))))

(define test2
  (lambda ()
    (initialize-dynmem)
    (mem-size! 200)
    (let ([doit
           (lambda (size)
             (let ([locs (list (malloc size)
                               (malloc size)
                               (malloc size))])
               (for-each
                (lambda (loc)
                  (mrelease loc))
                locs)))])
      (doit 20)
      (doit 20)
      (show-memory)
      (doit 10)
      (doit 20)
      (show-memory)
      )))
      



#|
(print-hash-table #t)
(let loop ([n *MAX-POOLS*])
  (printf "~a = ~a~n"
          n (hash-table-get 
             *POOL-SIZES* n (lambda () 'foo)))
  (if (> n 0)
      (loop (sub1 n))))
|#
