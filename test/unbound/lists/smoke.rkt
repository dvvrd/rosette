#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic a (listof integer?))

(define just+
  (λ/unbound (x y) (~> integer? integer? integer?)
             (+ x y)))

(define/unbound (+/abs x y) (~> integer? integer? integer?)
  (+ (abs x) y))

(define/typed (add1-to-acc x y) (~> integer? integer? integer?) (add1 y))

(foldl + 0 '(1 2 3 4))

(define smoke-tests
  (test-suite+
   "[unbound] Tests for lists/smoke.rkt"

   (check-sat
    (verify/unbound (assert (>= (foldl just+ 0 a) 0))))

   (check-unsat
    (verify/unbound (assert (>= (foldl +/abs 0 a) 0))))

   (check-unsat
    (verify/unbound (assert (>= (foldl (λ/typed (x y) (~> integer? integer? integer?) (+ (abs x) y)) 0 a) 0))))

   (check-sat
    (verify/unbound (assert (>  (foldl +/abs 0 a) 0))))

   (check-unsat
    (verify/unbound (assert (= (foldl add1-to-acc 0 a) (length a)))))

   (check-sat
    (verify/unbound (assert (= (foldl add1-to-acc 1 a) (length a)))))))

(time (run-tests smoke-tests))
