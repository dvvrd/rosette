#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic n m integer?)

(define/unbound (gcd n m) (~> integer? integer? integer?)
  (cond [(< n m) (gcd m n)]
        [(zero? n) m]
        [(zero? m) n]
        [else (gcd m (- n m))]))


(time
   (verify/unbound #:assume (assert (and (> n 0) (> m 0)))
                    #:guarantee (assert (= (gcd n m) (gcd m n)))))


