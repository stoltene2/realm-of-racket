(module position racket
  (provide (all-defined-out))

  (struct posn [x y] #:transparent)

  (define (posn=? a b)
    (and (= (posn-x a) (posn-x b))
         (= (posn-y a) (posn-y b))))

  )
