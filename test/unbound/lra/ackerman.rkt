#lang rosette/unbound

(current-bitwidth #f)

(define/unbound (a m n) (~> integer? integer? integer?)
  (cond
    [(<= m 0) (add1 n)]
    [(and (> m 0) (<= n 0)) (a (sub1 m) 1)]
    [else (a (sub1 m) (a m (sub1 n)))]))


(define-symbolic n m integer?)
(verify/unbound (assert (> (a m n) n)))
(verify/unbound (assert (> (a m n) 0)))
