#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define/unbound for-all (f) (xs) (~> (listof integer?) boolean?)
  (if (null? xs) (= (* 2 n) (+ n n))
      (and (f (car xs)) (for-all f (cdr xs)))))

(define (check x)
  (>= x 0))

(define/unbound (make-list n) (~> integer? (listof integer?))
  (if (< n 0)
      null
      (cons n (make-list (sub1 n)))))

; Expecting unsat
(time
 (verify/unbound
  (assert (for-all check (make-list n)))))
