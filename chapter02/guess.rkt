#lang racket

(define (guess)
  (quotient (+ higher lower) 2))

(define (smaller)
  (set! higher (max (sub1 (guess)) lower))
  (guess))

(define (bigger)
  (set! lower (min (add1 (guess)) higher))
  (guess))

(define (main a b)
  (set! lower (min a b))
  (set! higher (max a b))
  (guess))
