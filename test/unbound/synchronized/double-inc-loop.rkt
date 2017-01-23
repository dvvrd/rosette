#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic x1 x2 integer?)

(define/unbound (f1 i n) (~> integer? integer? integer?)
  (if (< i n) (+ 2 (f1 (+ i 1) n)) 0))

(time
   (verify/unbound #:assume    (assert (and (>= x2 x1) ))
                   #:guarantee (assert
                                 (let ([m1 (f1 1 x1)] [m2 (f1 1 x2)])
                                 (< m1 (+ m2 1))))))
