#lang rosette/unbound

; Expecting unsat and unsat

(current-bitwidth #f)

(define/unbound (id/boolean c) (~> boolean? boolean?)
  c)

(define/unbound (id/integer z) (~> integer? integer?)
  z)

(define-symbolic a b boolean?)
(define-symbolic x y integer?)

(verify/unbound #:assume (assert (and a (not b)))
                #:guarantee (assert (or (id/boolean a) (id/boolean b))))

(verify/unbound #:assume (assert (and (positive? x) (positive? y)))
                #:guarantee (assert (positive? (+ (id/integer x) (id/integer y)))))
