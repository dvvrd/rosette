#lang rosette/unbound

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(dbg-level 0)
(current-bitwidth #f)
(define-symbolic x y a b integer?)

(define/unbound (mult x y) (~> integer? integer? integer?)
  (if (= x 0) 0
      (+ y (mult (- x 1) y))))

(define/unbound (pwr a x) (~> integer? integer? integer?)
  (if (= a 0) 1
      (mult (pwr (- a 1) x) x)))

(define pwr-tests
  (test-suite+
   "[unbound] Tests for lra/power.rkt"

   (check-unsat
     (verify/unbound (assert (= (mult 23 10) 230)))
   )

   (check-sat
     (verify/unbound (assert (= (mult 23 11) 23)))
   )

   (check-sat
     (verify/unbound #:assume    (assert (and (> a 0) (> x 0)))
                     #:guarantee (assert (> (mult a x) a)))
   )

   (check-unsat
     (verify/unbound #:assume    (assert (and (> a 1) (> x 1)))
                     #:guarantee (assert (let ([b (mult a x)]) (and (> b a) (> b x)))))
   )

   (check-unsat
     (verify/unbound #:assume    (assert (and (> a 1) (> x 0)))
                     #:guarantee (assert (let ([b (mult a x)])(or (> b a) (> b x)))))
   )

   (check-unsat
     (verify/unbound (assert (= (pwr 3 2) 8)))
   )

   (check-sat
     (verify/unbound (assert (= (pwr 2 3) 8)))
   )

   (check-sat
     (verify/unbound #:assume    (assert (> x 2))
                     #:guarantee (assert (> (pwr 2 x) (mult 3 x))))
   )

   (check-unsat
     (verify/unbound #:assume    (assert (and (> x 0) (> a 0)))
                     #:guarantee (assert (> (pwr a x) 0)))
   )

   (check-unsat
     (verify/unbound #:assume    (assert (and (> a 2) (> x 1)))
                     #:guarantee (assert (> (pwr a x) (* 2 x))))
   )

   (check-unsat
    (verify/unbound #:assume (assert (and (> y x) (> x 0) (> a 0)))
                    #:guarantee (assert (> (pwr a y) (pwr a x)))))

   (check-sat
    (verify/unbound #:assume (assert (and (>= y x) (> x 0) (> a 0)))
                    #:guarantee (assert (> (pwr a y) (pwr a x)))))
))

(time (run-tests pwr-tests))
