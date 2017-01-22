#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define/unbound fact (exn) (n) (~> integer? integer?)
  (cond [(<= n 0) (exn 0)]
        [else
         (let ([exn
                (λ (n)
                  (if (zero? n) 1 (exn n)))])
           (* n (fact exn (- n 1))))]))
 
(define/unbound (exn n) (~> integer? integer?)
  (assert #f)
  1)

(define (trivial-query n)
  (= (+ n n) (* 2 n)))

; Expecting unsat
(verify/unbound
 #:assume (assert (> n 0))
 #:guarantee
 (assert
  (trivial-query
   (fact exn n))))
