#lang rosette/unbound

(dbg-level 0)
(define-symbolic n integer?)

(define (div x y)
  (assert (not (zero? y)))
  0)

(define/unbound (range i j) (~> integer? integer? (listof integer?))
  (cond
    [(> i j) null]
    [else
     (let ([is (range (add1 i) j)])
       (cons i is))]))

(define (harmonic n)
  (let ([ds (range 1 n)])
    (foldl (λ/typed (k s) (~> integer? integer? integer?)
                    (+ s (div 100000 k)))
           0 ds)))

(define (trivial-query x)
  (= (* 2 x) (+ x x)))

; Expecting unsat
(time
 (verify/unbound (assert (trivial-query (harmonic n)))))
