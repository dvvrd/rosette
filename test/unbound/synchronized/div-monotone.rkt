#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic a x y z v integer?)

(define/unbound (div x y) (~> integer? integer? integer?)
   (if (< x y) 0
      (+ 1 (div (- x y) y))))

(time
    (verify/unbound #:assume (assert (and (> y 0) (> x 0) (> x y) (> a 0)))
                    #:guarantee (assert (<=  (div a x) (div a y)))))

