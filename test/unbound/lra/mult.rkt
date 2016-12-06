#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic w x y z integer?)

(define/unbound (mult x y) (~> integer? integer? integer?)
  (if (= y 0) 0
      (+ x (mult x (- y 1)))))

(define mult-tests
  (test-suite+
   "[unbound] Tests for lra/mult.rkt"

   (check-sat
    (verify/unbound (assert (and (= 5 (mult 2 3)) (= 6 (mult 2 3))))))

   (check-unsat
    (verify/unbound (assert (= 6 (mult 2 3)))))

   (check-sat
    (verify/unbound (assert (= (mult x y) (mult w z)))))

   (check-unsat
    (verify/unbound (assert (= (mult x y) (mult x y)))))
   
   (check-sat
    (verify/unbound (assert (= (add1 (mult x y)) (mult x y)))))
))

(time (run-tests mult-tests))
