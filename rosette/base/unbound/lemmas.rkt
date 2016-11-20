#lang racket

(require (only-in "../core/term.rkt" constant solvable-domain solvable-range)
         (only-in "../core/safe.rkt" assert)
         (only-in "../../solver/solution.rkt" unsat?)
         (only-in "define.rkt" λtype λtyped?)
         (only-in "query.rkt" verify/unbound))

(provide associative?)

; Returns #t if applies 2 arguments of the type same as its range
; and for all x, y and z (f x (f y z)) = (f (f x y) z)
(define (associative? f)
  (with-handlers ([exn:fail? (λ (err) #f)])
    (let* ([type (λtype f 'associative?)]
           [domain (solvable-domain type)]
           [range (solvable-range type)])
      (and (equal? (length domain) 2)
           (equal? (car  domain) range)
           (equal? (cadr domain) range)
           (let* ([x (constant (gensym) range)]
                  [y (constant (gensym) range)]
                  [z (constant (gensym) range)]
                  [fxy (f x y)])
             (unsat? (verify/unbound
                      (assert
                       (equal?
                        (f fxy z)
                        (f z fxy))))))))))
