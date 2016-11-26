#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic x y a integer?)

(define/unbound (mult x y) (~> integer? integer? integer?)
  (if (= y 0) 0
      (+ x (mult x (- y 1)))))

(define/unbound (fact x) (~> integer? integer?)
  (if (= x 0) 1
      (mult x (fact (- x 1)))))

(define inc-tests
  (test-suite+
   "[unbound] Tests for lra/fact.rkt"

   (check-unsat
    (verify/unbound #:assume    (assert (> x 0))
                    #:guarantee (assert (> (fact x) 0))))

   (check-sat
    (verify/unbound #:assume    (assert (> x 0))
                    #:guarantee (assert (> (fact x) x))))

   (check-unsat
    (verify/unbound #:assume    (assert (>= x 1))
                    #:guarantee (assert (>= (fact x) x))))))

(time (run-tests inc-tests))