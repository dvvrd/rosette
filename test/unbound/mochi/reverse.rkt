#lang rosette/unbound

(dbg-level 0)
(define-symbolic len integer?)

(define li? (listof integer?))

(define/unbound (reverse acc xs) (~> li? li? li?)
  (if (null? xs)
      acc
      (reverse (cons (car xs) acc)
               (cdr xs))))

(define/unbound (make-list n) (~> integer? li?)
  (if (= n 0)
      null
      (cons n (make-list (sub1 n)))))

; Expecting unsat
(time
 (verify/unbound
  #:assume (assert (> len 0))
  #:guarantee (assert (> (length (reverse null (make-list len))) 0))))
