
(load "../common/common.ss")

(define (random-in-range low high)
    (let ( (range (- high low))  )
        (+ low (random range))))

(define (make-inner-circle rec)
    (let ( (center (rec 'center))
           (radius-1 (/ (rec 'width ) 2))
           (radius-2 (/ (rec 'height) 2)) )
        (let ( (radius (if (<= radius-1 radius-2)
                           radius-1
                           radius-2)) )
            (make-circle center radius))))

(define (estimate-integral rec trials)
    (* (monte-carlo trials in-circle rec) (rec 'area)))

(define (monte-carlo trials experiment rec)
    (define (iter trials-remaining trials-passed)
        (cond ( (= trials-remaining 0) (/ trials-passed trials)  )
              ( (experiment rec)
                    (iter (- trials-remaining 1) (+ trials-passed 1)) )
              ( else (iter (- trials-remaining 1) trials-passed) )))
    (iter trials 0))

(define (in-circle rec)
    (let ( (circle (make-inner-circle rec))
           (x (random-in-range (rec 'x1) (rec 'x2)))
           (y (random-in-range (rec 'y1) (rec 'y2)))  )
;        (display x) (display "  ") (display y) (newline)
        ((circle 'in) (make-point x y))))

;;; Experiment
(let ( (x1 (random 100)) (y1 (random 100))
       (width (random 100)) (height (random 100)) )
    (define rec (make-rectangle x1 y1 (+ x1 width) (+ y1 height)))

    (define estimate-pi (* 1.0 (/ (estimate-integral rec 50000)
                         (square ((make-inner-circle rec) 'radius)))))

    (display "The calculated pi is ")
    (display estimate-pi)
    (newline)
)
(exit)

