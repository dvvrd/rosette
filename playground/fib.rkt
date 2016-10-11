#lang rosette/unbound

(require "../rosette/base/core/term.rkt")

(current-bitwidth #f)
(define-symbolic m integer?)

(define/unbound (fib n) (~> integer? integer?)
  (define p 1)
  (let ([k 2])
    (displayln (format "working: ~a ~a" n (< n k)) (current-error-port))
    (if (< n (- k 1)) p (if (< n k) p (+ (fib (- n p)) (fib (- n k)))))))

(verify/unbound (assert (> (fib m) 0)))
