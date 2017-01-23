#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic x0 x1 x2 x3 x4 x5 x6 x7 x8 y0 y1 y2 y3 y4 y5 y6 y7 y8 integer?)

(define/unbound (f0 i j) (~> integer? integer? integer?)
  (if (> j 0) (+ i (f0 i (- j 1))) 0))

(time
   (verify/unbound #:assume    (assert (and (>= x0 0) (>= x1 x0) (>= x2 x1) (>= x3 x2) (>= x4 x3) (>= x5 x4) (>= x6 x5) (>= x7 x6) (>= x8 x7) (>= y1 y0) (>= y2 y1) (>= y3 y2) (>= y4 y3) (>= y5 y4) (>= y6 y5) (>= y7 y6) (>= y8 y7)))
                   #:guarantee (assert
                                 (let ([m0 (f0 x0 y0)] [m1 (f0 x1 y1)] [m2 (f0 x2 y2)] [m3 (f0 x3 y3)] [m4 (f0 x4 y4)] [m5 (f0 x5 y5)] [m6 (f0 x6 y6)] [m7 (f0 x7 y7)] [m8 (f0 x8 y8)])
                                 (<= m0 (+ m1 m2 m3 m4 m5 m6 m7 m8))))))

