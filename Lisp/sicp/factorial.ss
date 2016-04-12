
(define (factorial n)
    (define (fact-iter product counter max-count)
        (if (< counter 1)
            product
            (fact-iter (* counter product)
                       (- counter 1)
                        max-count)))
    (fact-iter 1 n n))

;; another one, using no define
(define factorial-2 (lambda (n)
    ((lambda (fact) (fact fact n))
     (lambda (ft k)
         (if (= k 1)
             1
             (* k (ft ft (- k 1))))))) )

(display "Factorial of 15 is ")
(display (factorial 15))
(newline)
(display "Factorial of 15 is ")
(display (factorial-2 15))
(newline)
(exit)
