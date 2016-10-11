#lang rosette/unbound

(current-bitwidth #f)
(define-symbolic n m integer?)

(define/unbound (gcd n m) (~> integer? integer? integer?)
  (cond [(< n m) (gcd m n)]
        [(zero? n) m]
        [(zero? m) n]
        [else (gcd m (- n m))]))

(gcd 30 75)
(verify/unbound #:assume (assert (and (> n 0) (> m 0)))
                #:guarantee (assert (> (gcd m n) 0)))
