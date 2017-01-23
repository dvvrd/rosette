#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define/unbound (make-list n) (~> integer? (listof integer?))
  (if (zero? n)
      null
      (cons n (make-list (sub1 n)))))

; Expecting unsat
(verify/unbound
 #:assume (assert (> n 0))
 #:guarantee
 (let ([xs (make-list n)])
   (assert
    (not (null? xs)))))
