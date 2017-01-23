#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic x1 x2 integer?)

(define/unbound (f1 n) (~> integer? integer?)
  (if (> n 0) (+ n n (f1 (- n 1))) 0))

(time
   (verify/unbound #:assume    (assert (>= x2 x1))
                   #:guarantee (assert
                                 (let ([m1 (f1 x1)] [m2 (f1 x2)])
                                 (<= m1 m2)))))
