#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define/unbound iter (f) (xs) (~> (listof integer?) boolean?)
  (cond
    [(null? xs) (= (* 2 n) (+ n n))]
    [else (f (car xs))
          (iter f (cdr xs))]))

(define (check x)
  (assert (>= x 0)))

(define/unbound (make-list n) (~> integer? (listof integer?))
  (if (< n 0)
      null
      (cons n (make-list (sub1 n)))))

; Expecting unsat
(time
 (verify/unbound
  (assert (iter check (make-list n)))))
