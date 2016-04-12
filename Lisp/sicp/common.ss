;;; constants
(define nil '())
(define true #t)
(define false #f)
(define empty "")

;;; utilities
(define (square x) (* x x))

(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (average x y) (/ (+ x y) 2))

(define (display-line x) (display x) (newline))

(define (logical-and s1 s2)
    (cond ( (or (= s1 0) (= s2 0)) 0 )
          ( (and (= s1 1) (= s2 1)) 1 )
          ( else (error "Invalid signal" (list s1 s2)))))

(define (logical-not s)
    (cond ( (= s 0) 1 )
          ( (= s 1) 0 )
          ( else (error "Invalid signal" s))))

(define (logical-or s1 s2)
    (cond ( (or (= s1 1) (= s2 1)) 1 )
          ( (and (= s1 0) (= s2 0)) 0 )
          ( else (error "Invalid signal" (list s1 s2)))))

(define (make-string val)
    (cond ( (null? val) empty )
          ( (string? val) val )
          ( (number? val) (number->string val) )
          ( (symbol? val) (symbol->string val) )
          ( else (error "Unrecognized data type" val) )
    ))

(define (range low high)
    (if (> low high) nil
        (cons low (range (+ low 1) high))))

(define (string-join str-list seperator)
    (cond ( (null? str-list) empty )
          ( (null? (cdr str-list)) (car str-list) )
          ( else (string-append (car str-list)
                                seperator
                                (string-join (cdr str-list) seperator)) )))

(define (find-if predict? lst)
    (cond ( (null? lst) #f )
          ( else (if (predict? (car lst)) (car lst)
                     (find-if predict? (cdr lst))) )))

(define (remove-if predict? lst)
    (cond ( (null? lst) lst )
          ( else (if (predict? (car lst)) (cdr lst)
                     (cons (car lst) (remove-if predict? (cdr lst)))) )))

;;; objects
(define (make-point x y)
    (lambda (op)
        (cond ( (eq? op 'x) x  )
              ( (eq? op 'y) y  )
              ( else (error "Invalid op -- make-point" op)  ))))
;; distance of two points from make-point
(define (distance p1 p2)
    (sqrt (+ (square (- (p1 'x) (p2 'x)))
             (square (- (p1 'y) (p2 'y))))))

(define (make-circle center radius)
    (define (in point)
        (<= (distance point center) radius))
    (define (dispatch m)
        (cond ( (eq? m 'center) center )
              ( (eq? m 'radius) radius )
              ( (eq? m 'in)     in )
              ( else (error "Invalid operation -- make-circle" m) )))
    dispatch)

(define (make-rectangle x1 y1 x2 y2)
;    (if (or (>= x1 x2) (>= y1 y2)) (error "Invalid rectangle"))
    (define (dispatch m)
        (cond ( (eq? m 'x1) x1 )
              ( (eq? m 'y1) y1 )
              ( (eq? m 'x2) x2 )
              ( (eq? m 'y2) y2 )
              ( (eq? m 'width) (- x2 x1) )
              ( (eq? m 'height) (- y2 y1) )
              ( (eq? m 'center)
                    (make-point (/ (+ x2 x1) 2) (/ (+ y2 y1) 2)) )
              ( (eq? m 'area) (* (- x2 x1) (- y2 y1)) )
              ( else (error "Invalid operation -- make-circle" m) )))
    dispatch)

