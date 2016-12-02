#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-symbolic n integer?)

(define-symbolic xs (listof integer?))

(define/typed (+/typed x y)
  (~> integer? integer? integer?)
  (+ x y))

(define/typed (-/typed x y)
  (~> integer? integer? integer?)
  (- y x))

(define/typed (+2x/typed x y)
  (~> integer? integer? integer?)
  (+ y x x))

(define a (foldl +/typed 0 xs))
(define b (foldl +/typed 0 xs))
(define c (foldl +2x/typed 0 xs))
(define d (foldl +/typed n xs))
(define e (foldl -/typed 0 xs))

(define foldeq-tests
  (test-suite+
   "[unbound] Tests for lists/foldeq.rkt"

   (check-unsat
     (verify/unbound (assert (= a b))))

   (check-sat
     (verify/unbound (assert (= (abs a) b))))

   (check-unsat
     (verify/unbound (assert (= (+ n a) d))))

   (check-unsat
     (verify/unbound (assert (= (- n e) d))))

   (check-unsat
     (verify/unbound (assert (= (+ a e) 0))))

   (check-unsat
     (verify/unbound (assert (= (* 2 a) c))))

   (check-unsat
     (verify/unbound (assert (= (abs a) (abs e)))))

   (check-sat
     (verify/unbound (assert (= a e))))

   (check-sat
     (verify/unbound (assert (= c d))))
))

(time (run-tests foldeq-tests))
