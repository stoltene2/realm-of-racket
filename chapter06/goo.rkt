(module goo racket
  (provide (all-defined-out))

  (require struct-update) ;struct-update-lib
  (require "position.rkt")

  (struct goo [loc expire] #:transparent)
  (define-struct-updaters goo)

  (define (can-eat head-pos goos)
    (cond [(empty? goos) #f]
          [else (if (close? head-pos (first goos))
                    (first goos)
                    (can-eat head-pos (rest goos)))]))

  (define (close? s goo)
    (posn=? s (goo-loc goo)))

  )
