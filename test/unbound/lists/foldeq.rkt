#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-symbolic xs (listof integer?))
(define/typed (+/typed x y)
  (~> integer? integer? integer?)
  (+ x y))

(define a (foldl +/typed 0 xs))
(define b (foldl +/typed 0 xs))

(define foldeq-tests
  (test-suite+
   "[unbound] Tests for lists/foldeq.rkt"

   (check-unsat
    (verify/unbound (assert (= a b))))
   (check-sat
    (verify/unbound (assert (= (abs a) b))))))

(time (run-tests foldeq-tests))
