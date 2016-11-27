#lang racket

(require
  (only-in "../core/bool.rkt" @boolean?)
  (only-in "../core/function.rkt" ~>)
  (only-in "../core/term.rkt"
           constant constant? expression @app
           type-of solvable-domain solvable-range type-applicable?)
  (only-in "dependencies.rkt" [read-dependencies/current read-dependencies] write-dependencies))

(provide relation? function-application->relation function->relation decompose-arguments
         (rename-out [fresh-relation-constant fresh-relation]))

(define relation-suffix "°")
(define relations-cache (make-hash))
(define args-decomposers (make-hash))

(define (relation? f)
  (and (constant? f)
       (string-suffix? (~a f) relation-suffix)
       (equal? (solvable-range (type-of f)) @boolean?)))

(define (function->relation f)
  (hash-ref relations-cache f #f))

(define (fresh-relation-constant name read-deps args write-deps ret)
  (let* ([read-deps# (length read-deps)]
         [args# (length args)]
         [write-deps# (length write-deps)]
         [decomposer (λ (xs)
                       (let*-values ([(read-deps+args write-deps+ret) (split-at xs (+ read-deps# args#))]
                                     [(read-deps-part args-part) (split-at read-deps+args read-deps#)]
                                     [(write-deps-part ret-part) (split-at write-deps+ret write-deps#)])
                         (values read-deps-part args-part write-deps-part ret-part)))]
         [result (constant (string->symbol (format "~a~a" name relation-suffix)) (argument-types->relation-type read-deps args write-deps ret))])
    (hash-set! args-decomposers result (λ (xs)
                                         (match xs
                                           [(? type-applicable?) (decomposer `(,@(solvable-domain xs) ,(solvable-range xs)))]
                                           [(? relation?) (decomposer `(,@(solvable-domain (type-of xs)) ,(solvable-range (type-of xs))))]
                                           [(expression (== @app) _ args ...) (decomposer args)]
                                           [(list xs ...) (decomposer xs)])))
    result))

(define (decompose-arguments f xs)
  ((hash-ref args-decomposers f) xs))

(define (fresh-relation f)
  (let* ([read-deps (map type-of (read-dependencies f))]
         [write-deps (map type-of (write-dependencies f))]
         [result (fresh-relation-constant f read-deps (solvable-domain (type-of f)) write-deps (solvable-range (type-of f)))])
    (hash-set! relations-cache f result)
    result))

(define (function-application->relation f read-deps args write-deps ret)
  (let ([rel (hash-ref! relations-cache f (thunk (fresh-relation f)))])
    (apply expression `(, @app ,rel ,@read-deps ,@args ,@write-deps ,ret))))

(define (argument-types->relation-type read-dependencies domain write-dependencies range)
  (apply ~> `(,@read-dependencies ,@domain ,@write-dependencies ,range , @boolean?)))

