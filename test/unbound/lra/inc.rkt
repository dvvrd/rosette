#lang rosette/unbound

(current-bitwidth #f)
(define-symbolic m n integer?)

(define/unbound (inc n) (~> integer? integer?)
  (+ n 1))

(define/unbound (dec n) (~> integer? integer?)
  (- n 1))

(define/unbound (inc->inc->dec n) (~> integer? integer?)
  (dec (inc (inc n))))

(verify/unbound #:assume    (assert (equal? n m))
                #:guarantee (assert (> (inc->inc->dec n) m)))
