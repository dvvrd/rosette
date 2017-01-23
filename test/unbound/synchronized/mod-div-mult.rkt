#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic a x y z v integer?)

(define/unbound (mod x y) (~> integer? integer? integer?)
  (if (< x y) x
      (mod (- x y) y)))

(define/unbound (div x y) (~> integer? integer? integer?)
   (if (< x y) 0
      (+ 1 (div (- x y) y))))

(define/unbound (mult x y) (~> integer? integer? integer?)
  (if (= x 0) 0
      (+ y (mult (- x 1) y))))

(time
    (verify/unbound #:assume (assert (and  (> x 0) (> a 0)))
                    #:guarantee (assert
                                 (let ([d (div a x)]
                                       [m (mod a x)]
                                       [p (+ v (mult z x))])
                                 (implies (and (= z d) (= v m)) (= a p))))))