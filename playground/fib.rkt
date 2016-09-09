#lang rosette/unbound

(current-bitwidth #f)
(define-symbolic m integer?)

(define (fib n)
  (let ([k 2])
    (displayln (format "working: ~a ~a" n (< n k)) (current-error-port))
    (if (< n k) 1 (+ (fib (- n 1)) (fib (- n 2))))))

(verify/unbound (assert (< (fib m) 0)))
