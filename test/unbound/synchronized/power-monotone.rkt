#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic a x y z integer?)

(define/unbound (mult x y) (~> integer? integer? integer?)
  (if (= x 0) 0
      (+ y (mult (- x 1) y))))

(define/unbound (pwr a x) (~> integer? integer? integer?)
  (if (= a 0) 1
      (mult (pwr (- a 1) x) x)))

(time
    (verify/unbound #:assume (assert (and (> y x) (> x 0) (> a 0)))
                    #:guarantee (assert (> (pwr a y) (pwr a x)))))

