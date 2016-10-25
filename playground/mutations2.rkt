#lang rosette/unbound

(current-bitwidth #f)

(define-symbolic n integer?)

(define x 0)
; TODO: why is this solved so hard?
(define/unbound (f n) (~> integer? integer?)
  (cond
    ; It was [(<= n 0) (set! x n) x] before, and the bug was found :)
    [(< n 0) (set! x n) x]
    [(equal? n 0) x]
    [else (set! x (add1 x))
          (f (sub1 n))]))
(f n)
(verify/unbound #:assume (assert (< n 20))
                #:guarantee (assert (equal? x n)))
