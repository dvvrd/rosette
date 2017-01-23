#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 1) ; will print gigabytes of rules

(define-symbolic xs (listof integer?))

(define/typed (add1 x) (~> integer? integer?) (+ x 1))
(define/typed (add-1 x) (~> integer? integer?) (- x 1))
(define/typed (add2 x) (~> integer? integer?) (+ x 2))
(define/typed (add-2 x) (~> integer? integer?) (- x 2))
(define/typed (add3 x) (~> integer? integer?) (+ x 3))
(define/typed (add-3 x) (~> integer? integer?) (- x 3))
(define/typed (add4 x) (~> integer? integer?) (+ x 4))
(define/typed (add-4 x) (~> integer? integer?) (- x 4))
(define/typed (add5 x) (~> integer? integer?) (+ x 5))
(define/typed (add-5 x) (~> integer? integer?) (- x 5))
(define/typed (add6 x) (~> integer? integer?) (+ x 6))
(define/typed (add-6 x) (~> integer? integer?) (- x 6))
(define/typed (add7 x) (~> integer? integer?) (+ x 7))
(define/typed (add-7 x) (~> integer? integer?) (- x 7))
(define/typed (add8 x) (~> integer? integer?) (+ x 8))
(define/typed (add-8 x) (~> integer? integer?) (- x 8))


(define/typed (+/typed x y) (~> integer? integer? integer?) (+ x y))


(time
  (verify/unbound
    (assert
      (= (+ (foldl +/typed 0 (map add1 xs))
            (foldl +/typed 0 (map add-2 xs))
            (foldl +/typed 0 (map add3 xs))
            (foldl +/typed 0 (map add-4 xs))
            (foldl +/typed 0 (map add-5 xs))
            (foldl +/typed 0 (map add6 xs))
            (foldl +/typed 0 (map add-7 xs))
            (foldl +/typed 0 (map add8 xs)))
         (+ (foldl +/typed 0 (map add-1 xs))
            (foldl +/typed 0 (map add2 xs))
            (foldl +/typed 0 (map add-3 xs))
            (foldl +/typed 0 (map add4 xs))
            (foldl +/typed 0 (map add5 xs))
            (foldl +/typed 0 (map add-6 xs))
            (foldl +/typed 0 (map add7 xs))
            (foldl +/typed 0 (map add-8 xs)))))))
