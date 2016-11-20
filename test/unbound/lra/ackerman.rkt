#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)

(define/unbound (a m n) (~> integer? integer? integer?)
  (cond
    [(<= m 0) (add1 n)]
    [(and (> m 0) (<= n 0)) (a (sub1 m) 1)]
    [else (a (sub1 m) (a m (sub1 n)))]))


(define-symbolic n m integer?)

(define ackerman-tests
  (test-suite+
   "[unbound] Tests for lra/ackerman.rkt"

   (check-unsat (verify/unbound (assert (> (a m n) n))))
   (check-sat (verify/unbound (assert (> (a m n) 0))))))

(time (run-tests ackerman-tests))
