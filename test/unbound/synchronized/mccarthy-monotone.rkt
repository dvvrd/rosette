#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic n m integer?)

(define/unbound (m91 n) (~> integer? integer?)
  (if (> n 100)
      (- n 10)
      (m91 (m91 (+ n 11)))))

(time
   (verify/unbound #:assume (assert (and (<= m n)))
                   #:guarantee (assert (<= (m91 m) (m91 n)))))