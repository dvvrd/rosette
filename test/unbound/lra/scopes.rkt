#lang rosette/unbound

(current-bitwidth #f)
(define-symbolic m integer?)
(define/unbound (f n) (~> integer? integer?)
  (define-symbolic k integer?)
  (if (>= k 0) (+ n m k) (+ n m)))

(verify/unbound #:assume    (assert (> m 0))
                #:guarantee (assert (> (f m) 0)))

(define/unbound (g n) (~> integer? boolean?)
  (equal? n m))

(define-symbolic a integer?)
(verify/unbound (assert (and (g m) (not (g a)))))
