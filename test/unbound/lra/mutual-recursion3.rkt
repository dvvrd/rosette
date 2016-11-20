#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)

(define-symbolic n integer?)
(define-symbolic y integer?)

(define x 2)
(set! y (abs y))

(define/unbound (f n) (~> integer? integer?)
  (cond
    [(> n 0) (set! y (+ y n))
             (f (g (- n 1)))]
    [else y]))

(define/unbound (g n) (~> integer? integer?)
  (set! x (add1 x))
  (set! y (+ y (f (- n 5))))
  y)

x
y
(f 5)
x
y

(define mutual-recursion3-tests
  (test-suite+
   "[unbound] Tests for lra/mutual-recursion3.rkt"

   (check-unsat
    (verify/unbound (assert (and (> (f n) 0) (> y 0)))))
   (check-unsat
    (verify/unbound (assert (and (> (f n) 10000000) (> y 10000000)))))))

(time (run-tests mutual-recursion3-tests))
