#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic a x y z v integer?)

(define/unbound (mod x y) (~> integer? integer? integer?)
  (if (< x y) x
      (mod (- x y) y)))

(time
    (verify/unbound #:assume (assert (and (> y 0) (> x 0) (> a 0)))
                    #:guarantee (assert (= (mod a y) (mod (+ a y) y)))))

