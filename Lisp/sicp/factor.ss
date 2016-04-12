
(define (factor n)
    (let f ( (n n) (i 2) )
        (cond
            ( (> i (sqrt n)) (list n) )
            ( (integer? (/ n i)) (cons i (f (/ n i) i)) )
            ( else (f n (+ i (if (even? i) 1 2))) )
        )))

(display "Factors of 360 are ")
(display (factor 360))
(newline)

(exit)
