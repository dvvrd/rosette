#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic x0 x1 x2 x3 x4 y0 y1 y2 y3 y4 integer?)

(define/unbound (f0 i j) (~> integer? integer? integer?)
  (if (> j 0) (+ i (f0 i (- j 1))) 0))

(time
   (verify/unbound #:assume    (assert (and (>= x0 0) (>= x1 x0) (>= x2 x1) (>= x3 x2) (>= x4 x3) (>= y1 y0) (>= y2 y1) (>= y3 y2) (>= y4 y3)))
                   #:guarantee (assert
                                 (let ([m0 (f0 x0 y0)] [m1 (f0 x1 y1)] [m2 (f0 x2 y2)] [m3 (f0 x3 y3)] [m4 (f0 x4 y4)])
                                 (<= m0 (+ m1 m2 m3 m4))))))
