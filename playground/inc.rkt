#lang rosette/unbound

(define-symbolic m n integer?)

(define/unbound (inc n) (~> integer? integer?)
  (if (>= m n) m (inc (add1 n))))

(verify/unbound #:assume (assert (< n (sub1 m))) #:guarantee (assert (< (inc n) m)))
