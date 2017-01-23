#lang rosette/unbound

; perhaps, depends on the right coverage

(current-bitwidth #f)
(dbg-level 0)
(define-symbolic n1 m1 n2 m2 integer?)
(define/unbound (a m n) (~> integer? integer? integer?)
  (cond
    [(<= m 0) (add1 n)]
    [(and (> m 0) (<= n 0)) (a (sub1 m) 1)]
    [else (a (sub1 m) (a m (sub1 n)))]))


(time
   (verify/unbound #:assume (assert (and (= n1 n2) (< m1 m2)))
                   #:guarantee (assert (<= (a n1 m1) (a n2 m2)))))
