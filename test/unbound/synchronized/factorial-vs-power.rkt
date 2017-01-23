#lang rosette/unbound

(current-bitwidth #f)
(ite-compactification #t)
(dbg-level 0)

(define-symbolic x integer?)

(define/unbound (mult x y) (~> integer? integer? integer?)
  (if (= x 0) 0
      (+ y (mult (- x 1) y))))

(define/unbound (fact a) (~> integer? integer?)
  (if (= a 0) 1
      (mult (fact (- a 1)) a)))

(define/unbound (pwr a x) (~> integer? integer? integer?)
  (if (= a 0) 1
      (mult (pwr (- a 1) x) x)))

(time
    (verify/unbound #:assume (assert (> x 1))
                    #:guarantee (assert (> (pwr x x) (fact x)))))

