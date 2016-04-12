
(load "../common/common.ss")

;;; 
(define (smallest-divisior n)
    (define (divides? a b)
        (= (remainder b a ) 0))
    (define (find-divisior n test-divisior)
        (cond ((> (square test-divisior) n) n)
              ((divides? test-divisior n) test-divisior)
              (else (find-divisior n (+ test-divisior 1)))))
    (find-divisior n 2))

(define (prime? n)
    (= n (smallest-divisior n)))

(define (fermat-test n)
    (define (try-it a)
        (= (remainder (expt a n) n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ( (= times 0) #t )
          ( (fermat-test n) (fast-prime? n (- times 1)))
          ( else #f )))

(define number 249823048)
(display "Using prime? ")
(time (display (prime? number)) (display " "))
(newline)
(display "Using fast-prime? ")
(time (display (fast-prime? number 5)) (display " "))
(newline)
(exit)
