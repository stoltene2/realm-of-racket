#lang racket

;; Type predicates help determine runtime types
(number? 12)
(number? "foo")
(integer? 12.0)
(cons? '(1 . 2))
(cons? '(1 2 3))

;; Determine if numbers are equal
(= 1 2)

;; Determine if strings are equal
(string=? "foo" "bar")

;; equal? compares basically everything
(equal? 'foo "foo")
(equal? 'foo 'foo)
(equal? "foo" "foo")

(define foo "foo")
(equal? "foo" foo) ;#t


;; If expression
(if (= (+ 1 2) 3)
    'yup-equal
    'not-equal)

;; When determining if something is true we choose anything not #f to
;; be true.

(if #f
    #t
    #f) ;#f

(if empty
    #t
    #f) ;#t

(if 1
    #t
    #f) ;#t

;; If is a special form that does not evaluate it's branches, it just returns them

(define if-result (if #t #t #f))


;; Conditions in racket

(define (special7 x)
  (cond [(= x 7) 5]
        [(odd? x) 'odd-number]
        [else 'even-number]))

(define (my-length x)
  (if (empty? x)
      0
      (add1 (my-length (rest x)))))

(define (my-length-cond x)
  (cond [(empty? x) 0]
        [else (add1 (my-length-cond (rest x)))]))

(= (my-length (range 0 10))
   (my-length-cond (range 0 10)))

(my-length-cond '(a 1 "foo" (student 'Me 123 'There) e))

;; This isn't tail recursive so it consumes stack space
(my-length (range 0 1000000))

;; Not sure if racket does tail call optimization. I'd need to figure
;; out how to profile this.
(define (my-length-acc x)
  (my-length-acc-1 0 x))

(define (my-length-acc-1 len x)
  (cond [(empty? x) len]
        [else (my-length-acc-1 (add1 len) (rest x))]))

(my-length-acc (range 0 1000000))
