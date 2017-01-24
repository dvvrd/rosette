#lang rosette/unbound

(dbg-level 0)
(define-symbolic m n integer?)

(define/unbound (make-list n) (~> integer? (listof integer?))
  (if (< n 0)
      null
      (cons n (make-list (sub1 n)))))

(define/typed (add x y) (~> integer? integer? integer?)
  (+ x y))

; Expecting unsat
(time
 (verify/unbound
  (assert
   (>= (foldl add m (make-list n)) m))))
