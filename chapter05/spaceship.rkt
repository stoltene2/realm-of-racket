#lang racket

(require 2htdp/universe 2htdp/image)

(define flame/orange-red (overlay/align "center" "bottom" (triangle 15 'solid 'orange) (triangle 30 'solid 'red)))
(define flame/red-orange (overlay/align "center" "bottom" (triangle 15 'solid 'red) (triangle 30 'solid 'orange)))
(define IMAGE-of-UFO  (bitmap/file "assets/ufo.png"))
(define image/ufo-1 (overlay/offset IMAGE-of-UFO 0 20 flame/orange-red))
(define image/ufo-2 (overlay/offset IMAGE-of-UFO 0 20 flame/red-orange))

(define WIDTH 200)
(define HEIGHT 600)

;;Initial falling velocity
(define VELOCITY 0)

;; Arbitrary downward acceleration
(define ACCELERATION 1/16)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct state [tick position] #:transparent)

(define (ship-image tick)
  (if (odd? tick)
      image/ufo-1
      image/ufo-2))


(define (update-state current-state)
  (state
   (add1 (state-tick current-state)) (calculate-pos current-state)))


(define (calculate-pos current-state)
  (define time (state-tick current-state))
  ;; v_0*t + 1/2*a*t^2
  (+ (* time VELOCITY) (* 1/2 ACCELERATION (sqr time))))


(define (draw-a-ufo-onto-an-empty-scene current-state)
  (place-image (ship-image (state-tick current-state)) (/ WIDTH 2) (state-position current-state)
               (empty-scene WIDTH HEIGHT)))


(define (state-matches-height current-state)
  (>= (state-position current-state) HEIGHT))


(big-bang (state 0 0)
  (on-tick update-state 1/40)
  (to-draw draw-a-ufo-onto-an-empty-scene)
  (stop-when state-matches-height))
