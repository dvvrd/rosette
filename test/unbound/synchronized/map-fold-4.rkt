#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)

(define-symbolic xs (listof integer?))

(define/typed (add1 x) (~> integer? integer?) ( + 1 (* x 2)))
(define/typed (add-1 x) (~> integer? integer?) ( - (* x 3) 1))
(define/typed (doub x) (~> integer? integer?) (* 5 x))
(define/typed (+/typed x y) (~> integer? integer? integer?) (+ x y))

(time
    (verify/unbound
     (assert
      (= (foldl +/typed 0 (map doub xs))
         (+ (foldl +/typed 0 (map add1 xs))
            (foldl +/typed 0 (map add-1 xs)))))))
