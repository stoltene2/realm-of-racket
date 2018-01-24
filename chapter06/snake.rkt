#lang racket

(require 2htdp/universe 2htdp/image)
(require struct-update) ;struct-update-lib
(require rackunit)

(require "goo.rkt")
(require "position.rkt")

(define TICK-RATE 1/16)
(define SIZE 29)
(define ENDGAME-TEXT-SIZE (* 2 SIZE))
(define EXPIRATION-TIME (* 10 (/ 1 TICK-RATE)))
(define EMPTY-SCENE (empty-scene (sqr SIZE) (sqr SIZE)))
(define SEG-SIZE SIZE)
(define INITAL-GOO-COUNT 10)
(define SEG-IMG (square SIZE "solid" "blue"))
(define GOO-IMG (square SIZE "solid" "green"))
(define HEAD-UP-IMG (square SIZE "solid" "red"))
(define HEAD-LEFT-IMG (square SIZE "solid" "red"))
(define HEAD-RIGHT-IMG (square SIZE "solid" "red"))
(define HEAD-DOWN-IMG (square SIZE "solid" "red"))

;;------------------------------------------------------------------------------
#|
Ideas

- Make a background tiling
- Make obstacles
- Make the goo change color as it ages
- Keep track of max length and score
- Make snake grow by more than a segment if needed

- Allow a configuration function to be passed into start so that we can control how things
  are created.
  - Different strategy for creating goos
|#


;
;
;   ;;;;          ;;
;   ;   ;         ;
;   ;    ;        ;
;   ;    ;  ;;;; ;;;  ;;;
;   ;    ; ;    ; ;  ;   ;
;   ;    ; ;;;;;; ;  ;;
;   ;    ; ;      ;    ;;;
;   ;   ;  ;;   ; ;  ;   ;
;   ;;;;    ;;;;  ;   ;;;;
;
;
;

(struct pit [snake goos] #:transparent)
(define-struct-updaters pit)

(struct snake [dir segs] #:transparent)
(define-struct-updaters snake)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-pit w)
  (define sn (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat (snake-head sn) goos))
  (if goo-to-eat
      (pit (grow sn) (age-goo (eat goos goo-to-eat)))
      (pit (slither sn) (age-goo goos))))


;; Keyboard navigation
(define (direct-snake w ke)
  (define (dir? x)
    (or [key=? x "up"]
        [key=? x "down"]
        [key=? x "left"]
        [key=? x "right"]))

  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))


(define (world-change-dir w d)
  (define sn (pit-snake w))
  (define turned-on-self? (opposite-dir? (snake-dir sn) d))
  (define has-body? (cons? (rest (snake-segs sn))))

  (cond [(and turned-on-self? has-body?) (stop-with w)]
        [else (pit (snake-change-dir sn d) (pit-goos w))]))

;; key-dir must be a direction
(define (opposite-dir? sn-dir key-dir)
  (define (opposite dir)
    (cond [(string=? dir "up") "down"]
          [(string=? dir "down") "up"]
          [(string=? dir "left") "right"]
          [(string=? dir "right") "left"]))
  (string=? sn-dir (opposite key-dir)))


(define (dead? w)
  (define sn (pit-snake w))
  (or (self-colliding? sn) (wall-colliding? sn)))

(define (self-colliding? sn)
  (cons? (member (snake-head sn) (snake-body sn))))

(define (wall-colliding? sn)
  (define x (posn-x (snake-head sn)))
  (define y (posn-y (snake-head sn)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))




;; TODO: Implement
(define (render-end w)
  (overlay (text "Game Over" ENDGAME-TEXT-SIZE "red")
           (render-pit w)))


;
;
;   ;;;;                 ;               ;;;;
;  ;   ;;                ;              ;    ;
;  ;    ;                ;   ;         ;      ;
;  ;;       ;;;;   ;;;;  ;  ;  ;;;;    ;      ;  ;   ;  ;;;;  ; ;;    ;
;    ;;;;   ;   ; ;    ; ; ;  ;    ;   ;      ;  ;   ; ;    ; ;;  ;  ;
;        ;  ;   ;     ;; ;;;  ;;;;;;   ;      ;  ;   ; ;;;;;; ;   ;  ;
;  ;     ;  ;   ; ;;;; ; ; ;; ;        ;   ;  ;  ;   ; ;      ;    ;;
;  ;;    ;  ;   ; ;    ; ;  ; ;;   ;    ;   ;;   ;   ; ;;   ; ;    ;;
;   ;;;;;   ;   ;  ;;;;; ;   ; ;;;;      ;;;;;    ;;;;  ;;;;  ;     ;
;                                             ;                    ;
;                                                                 ;;
;

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



;
;
;   ;;;;                 ;              ;;    ;;             ;
;  ;   ;;                ;              ;;    ;;             ;
;  ;    ;                ;   ;          ;;    ;;             ;
;  ;;       ;;;;   ;;;;  ;  ;  ;;;;     ; ;  ; ;  ;;;;   ;;;;;  ;;;
;    ;;;;   ;   ; ;    ; ; ;  ;    ;    ; ;  ; ; ;    ; ;   ;; ;   ;
;        ;  ;   ;     ;; ;;;  ;;;;;;    ; ;  ; ; ;    ; ;    ; ;;
;  ;     ;  ;   ; ;;;; ; ; ;; ;         ;  ;;  ; ;    ; ;    ;   ;;;
;  ;;    ;  ;   ; ;    ; ;  ; ;;   ;    ;  ;;  ; ;    ; ;   ;; ;   ;
;   ;;;;;   ;   ;  ;;;;; ;   ; ;;;;     ;  ;;  ;  ;;;;   ;;;;;  ;;;;
;
;
;

;;- Snake Modification Functions -----------------------------------------------

;; TODO: Review this
(define (snake-change-dir sn d)
  (snake-dir-set sn d))

(define (slither sn)
  (snake-segs-set sn (cons (next-head sn) (all-but-last (snake-segs sn)))))

;; NOTE: I had some problems with naming and variable shadowing. Error wasn't easy to debug.
;; I called an argument snake instead of sn
(define (grow sn)
  (snake-segs-set sn (cons (next-head sn) (snake-segs sn))))



;
;
;     ;;;;
;    ;   ;;
;   ;     ;
;   ;        ;;;;   ;;;;
;   ;  ;;;; ;    ; ;    ;
;   ;     ; ;    ; ;    ;
;   ;     ; ;    ; ;    ;
;    ;   ;; ;    ; ;    ;
;     ;;; ;  ;;;;   ;;;;
;
;
;

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))


(define (age-goo goos)
  (define (decay g)
    (goo-expire-update g sub1))

  (define (refresh g)
    (if (> (goo-expire g) 0)
        g
        (fresh-goo)))

  (map (compose refresh decay) goos))


(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       (add1 (random EXPIRATION-TIME))))


;
;
;   ;   ;         ;;;
;   ;   ;           ;
;   ;   ;   ;;;     ;    ;;;;    ;;;    ;;;;   ;;;
;   ;   ;  ;;  ;    ;    ;; ;;  ;;  ;   ;;  ; ;   ;
;   ;;;;;  ;   ;;   ;    ;   ;  ;   ;;  ;     ;
;   ;   ;  ;;;;;;   ;    ;   ;  ;;;;;;  ;      ;;;
;   ;   ;  ;        ;    ;   ;  ;       ;         ;
;   ;   ;  ;        ;    ;; ;;  ;       ;     ;   ;
;   ;   ;   ;;;;     ;;  ;;;;    ;;;;   ;      ;;;
;                        ;
;                        ;
;                        ;


(define (all-but-last lst)
  (define len (length lst))
  (cond [(empty? lst) lst]
        [else (take lst (sub1 len))]))


;
;
;   ;;;;;  ;;;;; ;;   ;  ;;;;   ;;;;;  ;;;;;  ;;;;; ;;   ;    ;;;
;   ;    ; ;     ;;   ;  ;   ;  ;      ;    ;   ;   ;;   ;   ;   ;
;   ;    ; ;     ;;;  ;  ;    ; ;      ;    ;   ;   ;;;  ;  ;
;   ;    ; ;     ; ;  ;  ;    ; ;      ;    ;   ;   ; ;  ;  ;
;   ;;;;;  ;;;;; ; ;; ;  ;    ; ;;;;;  ;;;;;    ;   ; ;; ;  ;   ;;
;   ;   ;  ;     ;  ; ;  ;    ; ;      ;   ;    ;   ;  ; ;  ;    ;
;   ;    ; ;     ;  ;;;  ;    ; ;      ;    ;   ;   ;  ;;;  ;    ;
;   ;    ; ;     ;   ;;  ;   ;  ;      ;    ;   ;   ;   ;;   ;   ;
;   ;     ;;;;;; ;   ;;  ;;;;   ;;;;;  ;     ;;;;;; ;   ;;    ;;;
;
;
;

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) EMPTY-SCENE)))

(define (snake+scene sn scene)
  (define snake-body-scene
    (img-list+scene (snake-body sn) SEG-IMG scene))
  (define dir (snake-dir sn))
  (img+scene (snake-head sn)
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))


(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

(define (goo-list+scene goos scene)
  (img-list+scene (map goo-loc goos) GOO-IMG scene))


;
;
;   ;;;;;     ;;    ;;    ;    ;;;;        ;;;;;     ;;    ;;    ;    ;;;;   ;
;   ;    ;    ;;    ;;    ;   ;   ;;       ;    ;    ;;    ;;    ;   ;   ;;  ;
;   ;    ;   ; ;    ; ;   ;  ;     ;       ;    ;   ; ;    ; ;   ;  ;     ;  ;
;   ;    ;   ;  ;   ; ;;  ;  ;             ;    ;   ;  ;   ; ;;  ;  ;        ;
;   ;;;;;;   ;  ;   ;  ;  ;  ;  ;;;;       ;;;;;;   ;  ;   ;  ;  ;  ;  ;;;;  ;
;   ;     ; ;;;;;;  ;  ;; ;  ;     ;  ;;;  ;     ; ;;;;;;  ;  ;; ;  ;     ;  ;
;   ;     ; ;    ;  ;   ; ;  ;     ;       ;     ; ;    ;  ;   ; ;  ;     ;
;   ;     ;;;    ;  ;    ;;   ;   ;;       ;     ;;;    ;  ;    ;;   ;   ;;
;   ;;;;;; ;     ;; ;    ;;    ;;; ;       ;;;;;; ;     ;; ;    ;;    ;;; ;  ;
;
;
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big Bang

(define (start-snake)
  (define initial-goos (map (λ (x) (fresh-goo)) (range 5)))
  (define initial-world (pit (snake "right" (list (posn 1 1)))
                             initial-goos))

  (big-bang initial-world
    (on-tick next-pit TICK-RATE)
    (on-key direct-snake)
    (to-draw render-pit)
    (stop-when dead? render-end)))



;
;
;  ;;;;;;;
;     ;                ;
;     ;                ;
;     ;    ;;;;  ;;;  ;;;  ;;;
;     ;   ;    ;;   ;  ;  ;   ;
;     ;   ;;;;;;;;     ;  ;;
;     ;   ;       ;;;  ;    ;;;
;     ;   ;;   ;;   ;  ;  ;   ;
;     ;    ;;;;  ;;;;  ;;  ;;;;
;
;
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(check-equal? (snake-head (snake "up" (list (posn 1 1) (posn 1 2))))
              (posn 1 1)
              "Snake head should return first element of segments")


;; (check-true (close? (posn 1 1) (goo (posn 1 1) 3))
;;               "Goo should match position")


;; (check-false (close? (posn 1 2) (goo (posn 1 1) 3))
;;              "Snake head should not be close to position")


;;- Eating goo -----------------------------------------------------------------
#|
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

|#
;;- Age Goos -------------------------------------------------------------------
(check-equal? (age-goo empty) empty
              "Aging empty goos yields empty goos")


(check-equal? (age-goo (list (goo (posn 1 1) 10)
                             (goo (posn 1 2) 9)))

              (list (goo (posn 1 1) 9)
                    (goo (posn 1 2) 8))

              "Decrease the count of each goo in the list")


(check-equal? (length (age-goo (list (goo (posn 1 1) 2)
                                    (goo (posn 1 2) 1))))

              2

              "Remove expired goo and add new")


;;- Changing directions --------------------------------------------------------

#|
> (define snake-going-left (snake "left" (list (posn 2 18) (posn 2 19))))
> (define plain-world (pit snake-going-left empty))
> (world-change-dir plain-world "right")
(stop-the-world (pit (snake "left" (list (posn 2 18) (posn 2 19))) '()))


(define snake-going-left (snake "left" (list (posn 2 18))))
> (define plain-world (pit snake-going-left empty))
> (world-change-dir plain-world "right")
(pit (snake "right" (list (posn 2 18))) '())
>

|#
