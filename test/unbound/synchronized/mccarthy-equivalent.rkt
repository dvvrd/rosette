#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic n integer?)

(define/unbound (m91 n) (~> integer? integer?)
  (if (> n 100)
      (- n 10)
      (m91 (m91 (+ n 11)))))

(define/unbound (m91-2 n) (~> integer? integer?)
  (if (< n 101)
      (m91-2 (m91-2 (+ n 11)))
      (- n 10)))

(time
   (verify/unbound (assert (= (m91 n) (m91-2 n)))))