
(load "../common/common.ss")

(define-syntax cons-stream
    (syntax-rules ()
        ((_ x y) (cons x (delay y)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-null? s) (null? s))
(define the-empty-stream nil)

(define (stream-ref s n)
    (if (= n 0) (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams)) the-empty-stream
        (cons-stream (apply proc (map stream-car argstreams))
                     (apply stream-map (cons proc
                                             (map stream-cdr argstreams))))))

(define (stream-foreach proc s)
    (if (stream-null? s) 'done
        (begin (proc (stream-car s))
               (stream-foreach proc (stream-cdr s)))))

(define (range-stream s low high)
  (map (lambda (index) (stream-ref s index))
        (range low high)))

(define (display-stream s)
    (stream-foreach display-line s))
(define (display-stream-n s n)
    (for-each (lambda (index) (display-line (stream-ref s index)))
        (range 0 n)))

(define (stream-filter pred stream)
    (cond ( (stream-null? stream) the-empty-stream )
          ( (pred (stream-car stream))
              (cons-stream (stream-car stream)
                           (stream-filter pred (stream-cdr stream))) )
          ( else (stream-filter pred (stream-cdr stream)))))

(define (stream-range low high)
    (if (> low high) the-empty-stream
        (cons-stream low (stream-range (+ low 1) high))))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define (scale-streams stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

;;; examples

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs
    (cons-stream 1 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
(define lucus
    (cons-stream 1 (cons-stream 3 (add-streams (stream-cdr lucus) lucus))))
(define factorials
    (cons-stream 1 (mul-streams integers factorials)))

;; squre root
(define (sqrt-improve guess x) (average guess (/ x guess)))
(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0
                    (stream-map (lambda (guess)
                                    (sqrt-improve guess x))
                                guesses)))
    guesses)

;; get pi
(define (partial-sums s)
    (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

(define (pi-summands n)
    (cons-stream (/ 1.0 n)
                 (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
    (scale-streams (partial-sums (pi-summands 1)) 4))

;; accelerator
(define (euler-transform s)
    (let ( (s0 (stream-ref s 0))
           (s1 (stream-ref s 1))
           (s2 (stream-ref s 2)) )
        (cons-stream (- s2 (/ (square (- s2 s1))
                              (+ s0 (* -2 s1) s2)))
                     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
    (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sqeuence transform s)
    (stream-map stream-car (make-tableau transform s)))

