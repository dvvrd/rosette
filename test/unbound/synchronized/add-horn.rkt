#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)

(define-symbolic x1 x2 y1 y2 integer?)

(define/unbound (f1 i j) (~> integer? integer? integer?)
  (if (= i j) j
      (f1 (- i 1) (+ j 1))))

(time
   (verify/unbound #:assume    (assert (and (>= x1 0) (>= x2 x1) (>= y2 y1)))
                   #:guarantee (assert
                                 (let ([z1 (f1 x1 y1)] [z2 (f1 x2 y2)])
                                 (< z1 (+ z2 1))))))


