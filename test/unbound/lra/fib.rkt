#lang rosette/unbound

(current-bitwidth #f)
(define-symbolic m integer?)

(define/unbound (fib n) (~> integer? integer?)
  (if (< n 2) 1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(verify/unbound (assert (> (fib m) 0)))
(verify/unbound (assert (> (fib m) 1)))
