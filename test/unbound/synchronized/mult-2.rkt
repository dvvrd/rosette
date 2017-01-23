#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic x0 x1 x2 y0 y1 y2  integer?)

(define/unbound (f0 i j) (~> integer? integer? integer?)
  (if (> j 0) (+ i (f0 i (- j 1))) 0))

(time
   (verify/unbound #:assume    (assert (and (>= x0 0) (>= x1 x0) (>= x2 x1) (>= y1 y0) (>= y2 y1)))
                   #:guarantee (assert
                                 (let ([m0 (f0 x0 y0)] [m1 (f0 x1 y1)] [m2 (f0 x2 y2)])
                                 (<= m0 (+ m1 m2))))))
