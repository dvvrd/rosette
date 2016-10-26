#lang racket

(require
  (only-in "../core/bool.rkt" @boolean?)
  (only-in "../core/function.rkt" ~>)
  (only-in "../core/term.rkt" constant constant? expression @app type-of solvable-domain solvable-range)
  (only-in "dependencies.rkt" [read-dependencies/current read-dependencies] write-dependencies))

(provide relation? function-application->relation)

(define relation-suffix "°")
(define relations-cache (make-hash))

(define (relation? f)
  (and (constant? f)
       (string-suffix? (format "~a" f) relation-suffix)
       (equal? (solvable-range (type-of f)) @boolean?)))

(define (fresh-relation f)
  (let* ([read-deps (map type-of (read-dependencies f))]
         [write-deps (map type-of (write-dependencies f))]
         [result (constant (string->symbol (format "~a~a" f relation-suffix))
                           (function-type->relational-type read-deps (type-of f) write-deps))])
    (hash-set! relations-cache f result)
    result))

(define (function-application->relation f read-deps args write-deps ret)
  (let ([rel (hash-ref! relations-cache f (thunk (fresh-relation f)))])
    (apply expression `(, @app ,rel ,@read-deps ,@args ,@write-deps ,ret))))

(define (function-type->relational-type read-dependencies t write-dependencies)
  (apply ~> `(,@read-dependencies ,@(solvable-domain t) ,@write-dependencies ,(solvable-range t) , @boolean?)))
