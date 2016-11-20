#lang rosette/unbound

(current-bitwidth #f)

(define-symbolic n integer?)
(define-symbolic xs (listof integer?))

(define/unbound (+/abs x y) (~> integer? integer? integer?)
  (+ (abs x) (abs y)))

(define lst (if (< n 0) (list 1 2 3) xs))
(verify/unbound (assert (>= (foldl +/abs 0 lst) 0)))
