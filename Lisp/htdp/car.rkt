#lang racket

(require 2htdp/image)

(define WHEEL-RADIUS 15)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define BODY-LENGTH (+ WHEEL-DISTANCE (* 6 WHEEL-RADIUS)))
(define BODY-HEIGHT (* WHEEL-RADIUS 2))

(define WHL (circle WHEEL-RADIUS "solid" "black"))
(define BDY
    (above
       (rectangle (/ BODY-LENGTH 2) (/ BODY-HEIGHT 2) "solid" "red")
       (rectangle BODY-LENGTH BODY-HEIGHT "solid" "red")))
(define SPC (rectangle WHEEL-DISTANCE 1 "solid" "white"))
(define WH* (beside WHL SPC WHL))
(define CAR (underlay/xy BDY WHEEL-RADIUS BODY-HEIGHT WH*))

