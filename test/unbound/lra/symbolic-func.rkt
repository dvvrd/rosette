#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-symbolic n integer?)

(define/unbound (f x) (~> integer? integer?)
  (+ x 1))
(define/unbound (g x) (~> integer? integer?)
  (+ x 2))

(define b (odd? n))
(define h
  (if b f g))

(define symbolic-func-tests
  (test-suite+
   "[unbound] Tests for lra/symolic-func.rkt"

   (check-unsat (verify/unbound (assert (even? (h n)))))
   (check-sat (verify/unbound (assert (odd? (h n)))))))

(time (run-tests symbolic-func-tests))
