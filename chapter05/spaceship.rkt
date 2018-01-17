#lang racket

(require 2htdp/universe 2htdp/image)

(define IMAGE-of-UFO  (bitmap/file "assets/ufo.png"))

(define WIDTH 200)
(define HEIGHT 200)

(struct game-state [tick])

(define (add-3-to-state current-state)
  (+ current-state 3))

(define (draw-a-ufo-onto-an-empty-scene current-state)
  (place-image IMAGE-of-UFO (/ WIDTH 2) current-state
               (empty-scene WIDTH HEIGHT)))

(define (state-matches-height current-state)
  (>= current-state HEIGHT))

(big-bang 0
  (on-tick add-3-to-state)
  (to-draw draw-a-ufo-onto-an-empty-scene)
  (stop-when state-matches-height))
