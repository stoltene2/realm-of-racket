#lang racket

;; Whole line comment
(+ 1 ;line comment
  2 #;(this form is ignored))

#|
  This is a block level comment

  Weeeeeeee
|#

;; #t and #f represent boolean values

'Foo ;Symbols start with an apostrophe
'Symbols-can-have-other.characters

;; Numbers
(symbol=? 'foo 'FOO)
; #f

(expt 42 42) ; Takes large expense of numbers using a BigNum

(sqrt -1)
; 0+1i

;; Floating point numbers have a decimal
(/ 1.0 2)
; 0.5

(/ 1 2)
; 1/2

;; Strings
(string-append "foo" "bar")
; "foobar"

;; Lists

(list 1 2 3)

'(1 2 3)

(equal? '(1 2 3) (list 1 2 3))
; #t

;; The value empty is '()
(equal? empty '())
; #t

;; Cons cells
#|
    Cons Cell '(val . val2)

      car             cdr
  +-------------+-------------+
  |             |             |
  |   val       |   val2      |
  |             |             |
  |             |             |
  +-------------+-------------+
|#

;; We use cons to construct lists and cons cells
(cons 'a (cons 'b (cons 'c empty)))

(cons 1 2) ;'(1 . 2)

(equal? (cons 'a (cons 'b (cons 'c empty))) '(a b c)) ; #t

;; Use first and rest functions to get at head and tails of lists
(first '(1 2 3)) ; 1

(rest '(1 2 3)) ; '(2 3)

;; Also, first, second, third functions exist
(first '(1 2 3))
(second '(1 2 3))
(third '(1 2 (4 5)))


;; structs
;; Structures have a structure definition
(struct student {name id# dorm} #:transparent)


#|
This creates for us the following functions,

student - to create new students
student-* - where * are the properties
student? - Tests if we have a student structure

|#

(define me (student 'Eric 1 'Home))
(student? me) ; #t
(student-name me) ;'Eric
(student-dorm me) ;'Home
(student-id# me) ;1

;; We can nest structures too
(define mimi (student 'Mimi 1234 'NewHall))
(define nicole (student 'Nicole 5678 'NewHall))
(define rose (student 'Rose 8765 'NewHall))
(define eric (student 'Eric 4321 'NewHall))
(define in-class (list mimi nicole rose eric))
(student-id# (third in-class)) ;8765
