#lang rosette/unbound

(current-bitwidth #f)
(define-symbolic n integer?)

(define/unbound (m91 n) (~> integer? integer?)
  (if (> n 100)
      (- n 10)
      (m91 (m91 (+ n 11)))))

(verify/unbound #:assume (assert (> n 100))
                #:guarantee (assert (equal? (m91 n) (- n 10))))

(verify/unbound #:assume (assert (<= n 100))
                #:guarantee (assert (equal? (m91 n) 91)))

(verify/unbound (assert (equal? (m91 n) 91)))
