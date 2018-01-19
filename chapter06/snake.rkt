#lang racket

(require 2htdp/universe 2htdp/image)

(require rackunit)

(define TICK-RATE 1)

;;------------------------------------------------------------------------------

(struct pit [snake goos] #:transparent)

(struct snake [dir segs] #:transparent)

(struct posn [x y] #:transparent)

(struct goo [loc expire] #:transparent)


;;------------------------------------------------------------------------------
(define (fresh-goo)
  (posn 1 1))


(define (render-pit w)
  (square 10 'solid 'blue))


(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat? snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))


(define (snake-head s)
  (first (snake-segs s)))

;; TODO: Implement
(define (direct-snake w key)
  (cond [else w]))


;; TODO: Implement
(define (dead? w)
  #f)

;; TODO: Implement
(define (can-eat? snake goos) #f)

;; TODO: Implement
(define (age-goo goos) goos)

;; TODO: Implement
(define (render-end w) empty)

;; TODO: Implement
(define (grow snake) snake)

;; TODO: Implement
(define (slither snake) snake)

;; TODO: Implement
(define (eat goos goo-to-eat) goos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big Bang

(define (start-snake)
  (define initial-world (pit (snake "right" (list (posn 1 1)))
                             (list (fresh-goo)
                                   (fresh-goo)
                                   (fresh-goo)
                                   (fresh-goo)
                                   (fresh-goo))))

  (big-bang initial-world
    (on-tick next-pit TICK-RATE)
    (on-key direct-snake)
    (to-draw render-pit)
    (stop-when dead? render-end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(check-equal? (snake-head (snake "up" (list (posn 1 1) (posn 1 2))))
              (posn 1 1)
              "Snake head should return first element of segments")

