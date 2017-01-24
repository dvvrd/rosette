#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

; TODO: interesting observation: if we increment x on 1 each iteration
; and try to prove x = y after all spacer can't accomplish it, though
; it solves current query (intuitively harder one)!

(dbg-level 0)
(merge-accuracy #f)
(current-bitwidth #f)

(define-symbolic n integer?)
(define x 0)
(define y 0)

(define/unbound (f n) (~> integer? integer?)
  (g n))

(define/unbound (g n) (~> integer? integer?)
  (set! x (+ x 2))
  (h n))

(define/unbound (h n) (~> integer? integer?)
  (set! y (add1 y))
  (cond [(<= n 0) 0]
        [else (g (sub1 n))]))

(define mutual-recursion1-tests
  (test-suite+
   "[unbound] Tests for lra/mutual-recursion1.rkt"

   (check-unsat
    (verify/unbound (assert (and (zero? (f n)) (equal? (abs x) (* 2 y))))))
   (check-sat
    (verify/unbound (assert (and (zero? (f n)) (equal? (add1 x) (* 2 y))))))))

(time (run-tests mutual-recursion1-tests))
