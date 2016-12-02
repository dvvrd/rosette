#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic x y a integer?)

(define/unbound (mult x y) (~> integer? integer? integer?)
  (if (= x 0) 0
      (+ y (mult (- x 1) y))))

(define/unbound (mult/neg x y) (~> integer? integer? integer?)
  (cond
    [(= x 0) 0]
    [(< x 0) (- (mult/neg (- x) y))]
    [else (+ y (mult/neg (- x 1) y))]
  ))

; GF: the following function is not recursive (as opposed to mult/neg),
;     but it still can be merged with mult (see the assertion below)
 (define/unbound (mult/neg2 x y) (~> integer? integer? integer?)
   (cond
     [(= x 0) 0]
     [(< x 0) (- (mult (- x) y))]
     [else (+ y (mult (- x 1) y))]
   ))
 
(verify/unbound #:assume    (assert (and (> x 0) (> y 0)))
                #:guarantee (assert (= (abs (mult x y)) (abs (mult/neg2 x y)))))

(define mult-neg-tests
  (test-suite+
   "[unbound] Tests for lra/mult-neg.rkt"

   (check-unsat
     (verify/unbound #:assume    (assert (and (< x 0) (> y 0)))
                     #:guarantee (assert (< (mult/neg x y) 0)))
   )

   (check-unsat
     (verify/unbound #:assume    (assert (and (> x 0) (< y 0)))
                     #:guarantee (assert (< (mult/neg x y) 0)))
   )

   (check-unsat
     (verify/unbound #:assume    (assert (and (> x 0) (> y 0)))
                     #:guarantee (assert (> (mult/neg x y) 0)))
   )

   (check-unsat
     (verify/unbound #:assume    (assert (and (< x 0) (< y 0)))
                     #:guarantee (assert (> (mult/neg x y) 0)))
   )

   (check-sat
     (verify/unbound #:assume    (assert (and (< x 0) (> y 0)))
                     #:guarantee (assert (= (mult/neg x y) 0)))
   )

   (check-sat
     (verify/unbound #:assume    (assert (and (< x 0) (> y 0)))
                     #:guarantee (assert (> (mult/neg x y) 0)))
   )

   (check-unsat
     (verify/unbound #:assume    (assert (and (> x 0) (> y 0)))
                     #:guarantee (assert (= (abs (mult x y)) (abs (mult/neg x y)))))
   )

   (check-sat
     (verify/unbound #:assume    (assert (and (> x 0) (> y 0)))
                     #:guarantee (assert (< (abs (mult x y)) (abs (mult/neg x y)))))
   )

   (check-unsat
     (verify/unbound #:assume    (assert (and (< x 0) (> y 0)))
                     #:guarantee (assert (= (mult x y) (- (mult/neg x y)))))
   )
))

(time (run-tests mult-neg-tests))