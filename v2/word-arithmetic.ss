(module word-arithmetic mzscheme
  (provide (all-defined))
  
  
(define-syntax word
  (syntax-rules ()
    [(word c)
     (cons (quotient c 65536) (remainder c 65536))]))
  
  
  
  ; PLT versions of the Larceny primitives
  (define-syntax fixnum? (syntax-rules () [(_ n) #t]))
  (define-syntax logand  (syntax-rules () [(_ . more) (bitwise-and . more)]))
  (define-syntax logior  (syntax-rules () [(_ . more) (bitwise-ior . more)]))
  (define-syntax logxor  (syntax-rules () [(_ . more) (bitwise-xor . more)]))
  (define-syntax lognot  (syntax-rules () [(_ . more) (bitwise-not . more)]))
  (define-syntax lsh     (syntax-rules () [(_ n s)    (arithmetic-shift n s)]))
  (define-syntax rshl    (syntax-rules () [(_ n s)    (arithmetic-shift n (- s))]))
                   
  ; Words are represented as a cons where the car holds the high 16
  ; bits and the cdr holds the low 16 bits.  Most good Scheme systems
  ; will have fixnums that hold at least 16 bits as well as fast
  ; allocation, so this has a fair chance at beating bignums for
  ; performance.

  (define (integer->word i)
    (if (or (and (fixnum? i) (>= i 0))
            (<= 0 i 4294967296))
        (cons (quotient i 65536) (remainder i 65536))
        (error "integer->word: out of range: " i)))

  (define (word->integer w)
    (+ (* (car w) 65536) (cdr w)))

  (define (word+ a b)
    (let ((t1 (+ (car a) (car b)))
          (t2 (+ (cdr a) (cdr b))))
      (cons (logand (+ t1 (rshl t2 16)) 65535)
            (logand t2 65535))))

  (define (word-or a b)
    (cons (logior (car a) (car b))
          (logior (cdr a) (cdr b))))

  (define (word-not a)
    (cons (logand (lognot (car a)) 65535) (logand (lognot (cdr a)) 65535)))

  (define (word-xor a b)
    (cons (logxor (car a) (car b)) (logxor (cdr a) (cdr b))))
    
  (define (word-and a b)
    (cons (logand (car a) (car b)) (logand (cdr a) (cdr b))))

  (define (word<<< a s)
    (define masks
      '#(#x0 #x1 #x3 #x7 #xF #x1F #x3F #x7F #xFF 
         #x1FF #x3FF #x7FF #xFFF #x1FFF #x3FFF #x7FFF #xFFFF))
    (define (rot hi lo s)
      (cons (logior (lsh (logand hi (vector-ref masks (- 16 s))) s)
                    (logand (rshl lo (- 16 s)) (vector-ref masks s)))
            (logior (lsh (logand lo (vector-ref masks (- 16 s))) s)
                    (logand (rshl hi (- 16 s)) (vector-ref masks s)))))
    (cond ((< 0 s 16)
           (rot (car a) (cdr a) s))
          ((< s 32)
           (rot (cdr a) (car a) (- s 16)))
          (else
           (error "word<<<: shift out of range: " s))))

  ;; word->bytes : word -> "(list byte byte byte byte)", little endian!
  (define (word->bytes w)
    (list (logand (cdr w) 255)
          (logand (rshl (cdr w) 8) 255)
          (logand (car w) 255)
          (logand (rshl (car w) 8) 255)))

  (define (word.4+ a b c d)
    (word+ (word+ (word+ a b) c) d))

  (define bitpos 
    '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
        26 27 28 29 30 31))

  (define powers 
    '#(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536
       131072 262144 524288 1048576 2097152 4194304 8388608 16777216 33554432
       67108864 134217728 268435456 536870912 1073741824 2147483648 
       4294967296))

  ;; word->bits : word -> (list (union 0 1))
  (define (word->bits w)
    (let ((w (word->integer w)))
      (define (bit i)  
        (remainder (quotient w (vector-ref powers i)) 2))
      (map bit bitpos)))

  ;; bits->integer : (list (union 0 1)) -> integer
  (define (bits->integer bs)
    (apply + (map * bs (map (lambda (i) (vector-ref powers i)) bitpos))))
  
  ;; Bytes and words
  ;; The least significant byte of a word is the first

  ;; bytes->word : (list byte*) -> word
  (define (bytes->word bs)
    (define (bs->w akk mul bs)
      (if (null? bs) 
          (integer->word akk)
          (bs->w (+ akk (* (car bs) mul)) (* 256 mul) (cdr bs))))
    (bs->w 0 1 bs))

  ;; bytes->words : (list byte) -> (list word)
  (define (bytes->words bytes)
    (define (loop bs l)
      (cond ((null? l)          (list (bytes->word (reverse bs))))
            ((< (length bs) 4)  (loop (cons (car l) bs)  (cdr l)))
            (else               (cons (bytes->word (reverse bs))
                                      (loop '() l)))))
    (if (null? bytes)
        '()
        (loop '() bytes)))

  ;; string->bytes : string -> (list byte)
  (define (string->bytes s)
    (map char->integer (string->list s)))
 
)