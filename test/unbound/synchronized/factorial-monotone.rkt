#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic a x y z integer?)

(define/unbound (mult x y) (~> integer? integer? integer?)
  (if (= x 0) 0
      (+ y (mult (- x 1) y))))

(define/unbound (fact a) (~> integer? integer?)
  (if (= a 0) 1
      (mult (fact (- a 1)) a)))

(time
    (verify/unbound #:assume (assert (and (> y x) (> x 0)))
                    #:guarantee (assert (> (fact y) (fact x)))))


