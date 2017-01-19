#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define/unbound (zip x y) (~> integer? integer? integer?)
  (if (zero? x)
      (if (zero? y)
          x
          (assert false))
      (if (zero? y)
          (assert false)
          (+ 1 (zip (- x 1) (- y 1))))))

(define/unbound (map x) (~> integer? integer?)
  (if (= x 0) x (+ 1 (map (- x 1)))))

; Expecting unsat
(verify/unbound (assert (= (map (zip n n)) n)))
