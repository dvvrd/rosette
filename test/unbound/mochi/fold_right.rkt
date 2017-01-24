#lang rosette/unbound

(dbg-level 0)
(define-symbolic m n integer?)

(define/unbound foldr (f) (xs acc) (~> (listof integer?) integer? integer?)
  (if (null? xs)
      acc
      (f (car xs) (foldr f (cdr xs) acc))))

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
   (>= (foldr add (make-list n) m) m))))
