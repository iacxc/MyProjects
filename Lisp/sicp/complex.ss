
(load "../common/common.ss")

(define (make-from-real-imag x y)

    (define (dispatch op)
        (cond ( (eq? op 'real-part) x )
              ( (eq? op 'imag-part) y )
              ( (eq? op 'magnitude)
                    (sqrt (+ (square x) (square y))) )
              ( (eq? op 'angle) (atan y x) )
              ( else
                  (error "Unknown op" op) )))
    dispatch)

(define c1 (make-from-real-imag 2 3))

(display (c1 'real-part)) (newline)
(display (c1 'magnitude)) (newline)

(display (c1 'angle)) (newline)

(exit)
