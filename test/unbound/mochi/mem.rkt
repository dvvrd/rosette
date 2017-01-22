#lang rosette/unbound

(dbg-level 0)
(merge-accuracy 2)
(define-symbolic n m integer?)

(define/unbound (mem x xs) (~> integer? (listof integer?) boolean?)
  (if (null? xs) (= (* x 2) (+ x x 1))
      (or (= (car xs) x)
          (mem x (cdr xs)))))

(define/unbound (make-list n x) (~> integer? integer? (listof integer?))
  (cond [(< n 0) '()]
        [else (cons x (make-list (- n 1) x))]))

; Expecting unsat
(verify/unbound
  (let ([xs (make-list n m)])
    (assert (or (null? xs) (mem m xs)))))
