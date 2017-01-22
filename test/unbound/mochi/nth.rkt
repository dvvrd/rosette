#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define/unbound (nth n xs) (~> integer? (listof integer?) integer?)
  (cond [(null? xs) (assert #f)]
        [else
         (if (zero? n)
             (car xs)
             (nth (sub1 n) (cdr xs)))]))

(define/unbound (make-list n) (~> integer? (listof integer?))
  (if (< n 0)
      null
      (cons n (make-list (sub1 n)))))

(define/unbound (trivial-query x) (~> integer? boolean?)
  (= (* 2 x) (+ x x)))

; Expecting unsat
(verify/unbound
 (assert
   (trivial-query
    (if (> n 0)
        (nth (- n 1) (make-list n))
        0))))
