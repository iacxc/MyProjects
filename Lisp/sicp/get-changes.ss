
;;; constants
(define nil '())

(define (get-changes amount coins)
    (define (cc amount coins lst)
        (cond ( (= amount 0) (list lst) )
              ( (or (< amount 0) (= (length coins) 0)) nil )
              ( else (append (cc amount (cdr coins) lst)
                             (cc (- amount (car coins))
                                 coins
                                 (cons (car coins) lst))))))
    (cc amount coins nil))

;; an example for get-changes
(define cn-coins (list 1 2 5 10))

(for-each (lambda (x) (display (reverse x)) (newline))
     (get-changes 20 cn-coins))

(time (get-changes 300 cn-coins))

(exit)
