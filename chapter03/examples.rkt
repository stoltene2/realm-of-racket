

;; Using list* attaches a sequence of elements to a fixed tail
(list* 1 2 3) ; (1 2 . 3)

;; Are these equal?
(equal? '(1 2 . 3) '(1 . (2 . 3))) ; #t
