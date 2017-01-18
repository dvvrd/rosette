#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-symbolic n integer?)

(define/unbound f (g) (n) (~> integer? integer?)
  (cond [(<= n 0) 0]
        [else (+ (g n) (f g (- n 1)))]))

(define/unbound (g n) (~> integer? integer?)
  (+ n n))
(define/unbound (h n) (~> integer? integer?)
  (- n n))

(define higher-order-tests
  (test-suite+
   "[unbound] Tests for lra/higher-order.rkt"


   (check-unsat (verify/unbound (assert (>= (f g n) 0))))
   (check-sat (verify/unbound (assert (> (f g n) 0))))
   (check-unsat (verify/unbound (assert (= (f h n) 0))))))

(time (run-tests higher-order-tests))

