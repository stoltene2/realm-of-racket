#lang racket

(require 2htdp/universe 2htdp/image)

(define IMAGE-of-UFO  (bitmap/file "assets/ufo.png"))

(define WIDTH 200)
(define HEIGHT 600)

;;Initial falling velocity
(define VELOCITY 0)

;; Arbitrary downward acceleration
(define ACCELERATION 1/8)

(struct state [tick position] #:transparent)

(define (update-state current-state)
  (state
   (add1 (state-tick current-state)) (calculate-pos current-state)))

(define (calculate-pos current-state)
  (define time (state-tick current-state))
  ;; v_0*t + 1/2*a*t^2
  (+ (* time VELOCITY) (* 1/2 ACCELERATION (sqr time))))

(define (draw-a-ufo-onto-an-empty-scene current-state)
  (place-image IMAGE-of-UFO (/ WIDTH 2) (state-position current-state)
               (empty-scene WIDTH HEIGHT)))

(define (state-matches-height current-state)
  (>= (state-position current-state) HEIGHT))

(big-bang (state 0 0)
  (on-tick update-state 1/60)
  (to-draw draw-a-ufo-onto-an-empty-scene)
  (stop-when state-matches-height))
