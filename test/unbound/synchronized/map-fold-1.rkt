#lang rosette/unbound

(current-bitwidth #f)
(dbg-level 0)

(define-symbolic xs (listof integer?))

(define/typed (add1 x) (~> integer? integer?) (+ x 1))
(define/typed (+/typed x y) (~> integer? integer? integer?) (+ x y))

(time
    (verify/unbound
     (assert
      (= (foldl +/typed 0 (map add1 xs))
         (+ (foldl +/typed 0 xs)
            (length xs))))))
