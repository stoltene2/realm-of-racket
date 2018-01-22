#lang racket

(require 2htdp/universe 2htdp/image)
(require struct-update) ;struct-update-lib

(require rackunit)

(define TICK-RATE 1)
(define SIZE 10)
(define EXPIRATION-TIME 10)

;;------------------------------------------------------------------------------

(struct pit [snake goos] #:transparent)

(struct snake [dir segs] #:transparent)

(struct posn [x y] #:transparent)

(struct goo [loc expire] #:transparent)
(define-struct-updaters goo)

;;------------------------------------------------------------------------------

(define (fresh-goo)
  (goo (posn
        (add1 (random (sub1 SIZE)))
        (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))


(define (render-pit w)
  (square 10 'solid 'blue))


(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))


;; TODO: Implement
(define (direct-snake w key)
  (cond [else w]))


;; TODO: Implement
(define (dead? w)
  #f)


(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))


(define (age-goo goos)
  (define (decay g)
    (goo-expire-update g sub1))

  (define aged (map decay goos))

  ;;TODO: Replace rotten goo with a new random goo
  (filter (lambda (x) (> (goo-expire x) 0))
          aged))


(define (grow snake)
  (snake (snake-dir snake))
  (cons (next-head snake) (snake-segs snake)))


(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

;; TODO: Implement
(define (render-end w) empty)

;;- Snake Query Functions ------------------------------------------------------

(define (snake-head sn)
  (first (snake-segs sn)))


(define (snake-body sn)
  (rest (snake-segs sn)))


(define (snake-tail sn)
  (last (snake-segs sn)))


(define (next-head sn)
  (define head (snake-head sn))
  (define dir  (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))


(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

;;- Snake Modification Functions -----------------------------------------------

;; TODO: Review this
(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head snake) (all-but-last (snake-segs sn)))))


;;- Helper functions -----------------------------------------------------------

(define (close? s goo)
  (posn=? s (goo-loc goo)))


(define (posn=? a b)
  (and (= (posn-x a) (posn-x b))
       (= (posn-y a) (posn-y b))))


(define (all-but-last lst)
  (define len (length lst))
  (cond [(empty? lst) lst]
        [else (take lst (sub1 len))]))

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


(check-true (close? (posn 1 1) (goo (posn 1 1) 3))
              "Goo should match position")


(check-false (close? (posn 1 2) (goo (posn 1 1) 3))
             "Snake head should not be close to position")


;;- Eating goo -----------------------------------------------------------------
(check-false (can-eat (snake "up" (list (posn 1 1))) empty)
             "Snakes cannot eat when there are no goos")

(check-false (can-eat (snake "up" (list (posn 1 1))) (list (goo (posn 1 2) 1)))
             "Snakes cannot eat when the snake head is not on a goo")

(check-equal? (can-eat (snake "up" (list (posn 1 1))) (list (goo (posn 1 1) 1)))
              (goo (posn 1 1) 1)
             "Snakes can eat if they are on the goo")

(check-equal? (can-eat (snake "up" (list (posn 1 1)))
                       (list (goo (posn 1 2) 1)
                             (goo (posn 1 1) 1)))
              (goo (posn 1 1) 1)
              "Snakes can eat if they are on any of the goos in the list")

;;- Age Goos -------------------------------------------------------------------
(check-equal? (age-goo empty) empty
              "Aging empty goos yields empty goos")


(check-equal? (age-goo (list (goo (posn 1 1) 10)
                             (goo (posn 1 2) 9)))

              (list (goo (posn 1 1) 9)
                    (goo (posn 1 2) 8))

              "Decrease the count of each goo in the list")


(check-equal? (age-goo (list (goo (posn 1 1) 2)
                             (goo (posn 1 2) 1)))

              (list (goo (posn 1 1) 1))

              "Remove expired goo from the list")
