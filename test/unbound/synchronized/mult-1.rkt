#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic x0 x1 y0 y1 integer?)

(define/unbound (f0 i j) (~> integer? integer? integer?)
  (if (> j 0) (+ i (f0 i (- j 1))) 0))


(time
   (verify/unbound #:assume    (assert (and (>= x0 0) (>= x1 x0) (>= y1 y0)))
                   #:guarantee (assert
                                 (let ([m0 (f0 x0 y0)] [m1 (f0 x1 y1)])
                                 (<= m0 m1)))))
