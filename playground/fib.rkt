#lang rosette/unbound

(current-bitwidth #f)
(define-symbolic m integer?)

(define (fib n)
  (let ([k 2])
    (if (< n k) 1 (+ (fib (- n 1)) (fib (- n 2))))))

;(define (f n) (case-lambda
;            [() 10]
;            [(x) x]
;            [(x y) (list y x)]
;            [r r]))
;
;    (list ((f 1) 1)
;          ((f 1) 1)
;          ((f 1) 1 2)
;          ((f 1) 1 2 3))
