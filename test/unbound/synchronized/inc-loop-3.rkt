#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic i n n1 n2 n3 n4  integer?)

(define/unbound (inc1 i n) (~> integer? integer? integer?)
  (if (< i n) (+ 1 (inc1 (+ i 1) n)) 0))

(time
   (verify/unbound #:assume    (assert (and (> n1 0) (> n2 0) (> n3 0) (> n4 0)))
                   #:guarantee (assert
                                 (let ([m1 (inc1 0 n1)] [m2 (inc1 0 n2)] [m3 (inc1 0 n3)] [m4 (inc1 0 n4)])
                                 (> (+ m1 m2 m3 m4) 0)))))
