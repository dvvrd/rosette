#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)

(define-symbolic xs (listof integer?))

(define/typed (add1 x) (~> integer? integer?) (+ x 1))
(define/typed (add-1 x) (~> integer? integer?) (- x 1))
(define/typed (add2 x) (~> integer? integer?) (+ x 2))
(define/typed (add-2 x) (~> integer? integer?) (- x 2))
(define/typed (add3 x) (~> integer? integer?) (+ x 3))
(define/typed (add-3 x) (~> integer? integer?) (- x 3))
(define/typed (add4 x) (~> integer? integer?) (+ x 4))
(define/typed (add-4 x) (~> integer? integer?) (- x 4))
(define/typed (+/typed x y) (~> integer? integer? integer?) (+ x y))

(time
    (verify/unbound
     (assert
      (= (+ (foldl +/typed 0 (map add2 xs))
            (foldl +/typed 0 (map add-2 xs))
            (foldl +/typed 0 (map add3 xs))
            (foldl +/typed 0 (map add-3 xs)))
         (+ (foldl +/typed 0 (map add1 xs))
            (foldl +/typed 0 (map add-1 xs))
            (foldl +/typed 0 (map add4 xs))
            (foldl +/typed 0 (map add-4 xs)))))))
