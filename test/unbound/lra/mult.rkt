#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic x y a integer?)

(define/unbound (mult x y) (~> integer? integer? integer?)
  (if (= y 0) 0
  (+ x (mult x (- y 1))))
)

(define/unbound (mult_acc x y a) (~> integer? integer? integer? integer?)
  (if (= y 0) a
  (mult_acc x (- y 1) (+ a x)))
)

(verify/unbound #:assume    (assert (>= y 0))
                #:guarantee (assert (= (+ a (mult x y)) (mult_acc x y a))))
