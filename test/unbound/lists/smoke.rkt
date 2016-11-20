#lang rosette/unbound

; Expecting sat, unsat, unsat and sat

(current-bitwidth #f)
(define-symbolic a (listof integer?))

(define just+
  (λ/unbound (x y) (~> integer? integer? integer?)
             (+ x y)))
(define/unbound (+/abs x y) (~> integer? integer? integer?)
  (+ (abs x) y))

(foldl + 0 '(1 2 3 4))

(verify/unbound (assert (>= (foldl just+ 0 a) 0)))
(verify/unbound (assert (>= (foldl +/abs 0 a) 0)))
(verify/unbound (assert (>= (foldl (λ/typed (x y) (~> integer? integer? integer?) (+ (abs x) y)) 0 a) 0)))
(verify/unbound (assert (>  (foldl +/abs 0 a) 0)))
