#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)

(define-symbolic n integer?)
(define-symbolic y integer?)

(define x 2)
(set! y 1)
(set! x 3)

(define/unbound (f n) (~> integer? integer?)
  (cond
    [(> n 0) (set! y (+ y n))
             (abs (g (- n 1)))]
    [else y]))

(define/unbound (g n) (~> integer? integer?)
  (set! x (add1 x))
  (set! y (+ y (f (- n 5))))
  n)

x
y
(f 5)
x
y

(define mutations1-tests
  (test-suite+
   "[unbound] Tests for lra/mutations1.rkt"

   (check-unsat
    (verify/unbound (assert (and (>= (f n) 0) (> y 0)))))

   (check-sat
    (verify/unbound (assert (and (> (f n) 0) (> y 1)))))))

(time (run-tests mutations1-tests))
