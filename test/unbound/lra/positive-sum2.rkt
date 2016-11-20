#lang rosette/unbound

; Expecting unsat and sat

(current-bitwidth #f)

(define/unbound (f n) (~> integer? integer?)
  (cond [(> n 0)
         (define-symbolic k integer?)
         (+ (abs k) (f (sub1 n)))]
        [else 0]))

(define-symbolic m integer?)
(verify/unbound (assert (>= (f m) 0)))
(verify/unbound (assert (> (f m) 0)))
