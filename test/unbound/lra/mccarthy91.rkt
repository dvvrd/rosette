#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic n integer?)

(define/unbound (m91 n) (~> integer? integer?)
  (if (> n 100)
      (- n 10)
      (m91 (m91 (+ n 11)))))

(define m91-tests
  (test-suite+
   "[unbound] Tests for lra/mccarthy91.rkt"

   (check-unsat
    (verify/unbound #:assume (assert (> n 100))
                    #:guarantee (assert (equal? (m91 n) (- n 10)))))

   (check-unsat
    (verify/unbound #:assume (assert (<= n 100))
                    #:guarantee (assert (equal? (m91 n) 91))))

   (check-sat
    (verify/unbound (assert (equal? (m91 n) 91))))))

(time (run-tests m91-tests))
