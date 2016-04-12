
;;; Standard procdures
;(define (expt b n)
;    (if (= n 0) 1
;        (* b (expt b (-n 1)))))
;(define (gcd a b)
;    (if (=b 0) a
;        (gcd b (remainder a b))))
;(define (sqrt x)
;    (define (good-enough? guess)
;        (< (abs (- (square guess) x)) 0.001))
;    (define (improve guess)
;        (average guess (/ x guess)))
;    (define (sqrt-iter guess)
;        (if (good-enough? guess)
;            guess
;            (sqrt-iter (improve guess))))
;    (sqrt-iter 1.0))
;;;

;;; Constants
(define nil '())
(define true #t)
(define false #f)

;;; Prodecures, ordered from a-z
(define (accumulate-n op init seqs)
    (if (null? (car seqs)) nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

(define (fast-expt b n)
    (define (expt-iter b counter product)
        (if (= counter 0) product
            (expt-iter b (- counter 1) (* b product))))
    (expt-iter b n 1))


(define (fibonacci n)
    (if (= n 0)
        0
        (let fib ( (i n) (a1 1) (a2 0) )
            (if (= i 1) a1
                (fib (- i 1) (+ a1 a2) a1))
            )))

(define (filter predicate sequence)
    (cond ( (null? sequence) nil )
          ( (predicate (car sequence))
                (cons (car sequence) (filter predicate (cdr sequence))))
          ( else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (integral f a b dx)
    (* (sigma f
              (+ a (/ dx 2.0))
              (lambda (x) (+ x dx))
              b)
        dx))

(define (lazy t )
    (let ((val #f) (flag #f))
        (lambda ()
            (if (not flag)
                (begin (set! val ( t ))
                       (set! flag #t)))
            val)))

; get a list of all leaves of a tree
(define (leaves tree)
    (cond ( (null? tree) nil )
          ( (not (pair? tree)) (list tree) )
          ( else (append (leaves (car tree))
                         (leaves (cdr tree))))))

(define (nth n lst)
    (cond ( (or (< n 0) (not (pair? lst))) #f )
          ( (= n 0) (car lst) )
          ( else (nth (- n 1) (cdr lst)))))

; a reduce with an initial value
(define (reduce op initial sequence)
    (define (iter result rest)
        (if (null? rest) result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

(define (reverse-1 sequence)
    (accumulate (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-2 sequence)
    (reduce (lambda (x y) (cons y x)) nil sequence))

; a general sum
(define (sigma term a next b)
    (if (> a b) 0
        (+ (term a) (sigma term (next a) next b))))

(define (search wanted? lst)
    (call/cc (lambda (return)
        (for-each (lambda (element)
                      (if (wanted? element)
                          (return element)))
                  lst)
        #f)))

(define (search-2 treat lst)
    (call/cc (lambda (return)
        (for-each (lambda (element)
                      (treat element return))
                  lst)
        #f)))

(define (square x) (* x x))

; get a list of square of each item from a list
(define (square-list items)
    (define (iter things answer)
        (if (null? things) answer
            (iter (cdr things)
                  (append answer (list (square (car things))) ))))
    (iter items nil))


;;; Excerises
(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms)
                        (+ this-coeff (* x higher-terms)))
                0
                coefficient-sequence))

(define empty-board nil)
(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0) (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (range 1 board-size)))
                (queen-cols (- k 1))))))
    (queen-cols board-size))

;;; Examples
(define (frame-coord-map frame)
    (lambda (v)
        (add-vect (origin-frame frame)
                  (add-vect (scale-vect (xcor-vect v)
                                        (edge1-frame frame))
                            (scale-vect (ycor-vect v)
                                        (edge2-frame frame))))))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1) (xcor-vect v2))
               (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1) (xcor-vect v2))
               (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v s)
    (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
              (if (same-variable? exp var) 1 0))
          ((sum? exp)
              (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
          ((product? exp)
              (make-sum
                  (make-product (multiplier exp) (deriv (multiplicand exp) var))
                  (make-product (deriv (multiplier exp) var) (multiplicand exp))))
          (else
              (error "unknown expression type --DERIV" exp))))
(define (tree->list-1 tree)
    (if (null? tree) nil
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree) result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
    (copy-to-list tree nil))

(define (partial-tree elts n)
    (if (= n 0) (cons nil elts)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
                (let ((left-tree (car left-result))
                      (non-left-elts (cdr left-result))
                      (right-size (- n (+ left-size 1))))
                (let ((this-entry (car non-left-elts))
                      (right-result (partial-tree (cdr non-left-elts) right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

;; an example for search
(define (treat element like-it)
    (if (zero? element)
        (like-it 'fnord)))

(search-2 treat '(1 2 3 0))

(define (make-withdraw balance)
    (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount) blance))
            "Insufficient funds")))

(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount) blance))
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch m)
        (cond ( (eq? m 'withdraw) withdraw )
              ( (eq? m 'deposit) deposit )
              ( else (error "Unknown request -- make-account" m))))
    dispatch)

(define (make-withdraw initial-amount)
    ((lambda (balance)
        (lambda (amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount)) balance)
                "Insufficient funds")))
    initial-amount))


