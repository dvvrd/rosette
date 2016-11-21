#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define/typed (+/typed x y) (~> integer? integer? integer?)
  (+ x y))

(define/typed (+/typed1 x y) (~> integer? integer? integer?)
  (+ x y x (- x)))

(define/typed (+/abs x y) (~> integer? integer? integer?)
  (+ (abs x) y))

(define-symbolic xs ys (listof integer?))
(define zs (append xs ys))

(define a (foldl +/typed 0 zs))
(define b (+ (foldl +/typed 0 xs)
             (foldl +/typed1 0 ys)))

(define c (+ (car xs) (car ys)))
(define d (foldl +/abs 0 zs))

(define append-sum-tests
  (test-suite+
   "[unbound] Tests for lists/append-sum.rkt"

   (check-unsat
    (verify/unbound (assert (= a b))))
   (check-sat
    (verify/unbound (assert (= (abs a) b))))

   (check-unsat
    (verify/unbound (assert (>= d c))))
   (check-sat
    (verify/unbound (assert (> d c))))))

(time (run-tests append-sum-tests))