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

(define (trivial-query x)
  (= (* 2 x) (+ x x)))

; Expecting unsat
(time
 (let ([xs (make-list n)])
   (verify/unbound
    #:assume (assert (not (null? xs)))
    #:guarantee (assert (trivial-query (nth 0 xs))))))
