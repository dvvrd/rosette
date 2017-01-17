#lang racket

(require
  (only-in "../core/bool.rkt" @boolean?)
  (only-in "../core/function.rkt" ~>)
  (only-in "../core/term.rkt"
           constant constant? expression @app
           type-of term-type solvable-domain solvable-range
           type? type-applicable?)
  (only-in "dependencies.rkt" [read-dependencies/current read-dependencies] write-dependencies))

(provide relation? function-application->relation function->relation
         decompose-arguments relation-suffix
         (rename-out [fresh-relation-constant fresh-relation]))

(define relation-suffix "°")
(define relations-cache (make-hash))
(define relation-symbols (mutable-seteq))
(define args-decomposers (make-hash))

(define (relation? f)
  (set-member? relation-symbols f))
;  (and (constant? f)
;       (string-suffix? (constant-name f) relation-suffix)
;       (equal? (solvable-range (term-type f)) @boolean?)))

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
         [result (constant (string->symbol (format "~a~a" name relation-suffix))
                           (argument-types->relation-type read-deps args write-deps ret))])
    (hash-set! args-decomposers result (λ (xs)
                                         (match xs
                                           [(and (? type?) (? type-applicable?)) (decomposer (solvable-domain xs))]
                                           [(? relation?) (decomposer (solvable-domain (term-type xs)))]
                                           [(expression (== @app) _ args ...) (decomposer args)]
                                           [(list xs ...) (decomposer xs)])))
    (set-add! relation-symbols result)
    result))

(define (decompose-arguments f xs)
  ((hash-ref args-decomposers f) xs))

(define (fresh-relation f)
  (let* ([read-deps (map type-of (read-dependencies f))]
         [write-deps (map type-of (write-dependencies f))]
         [result (fresh-relation-constant f read-deps (solvable-domain (term-type f)) write-deps (list (solvable-range (term-type f)) @boolean?))])
    (hash-set! relations-cache f result)
    result))

(define (function-application->relation f read-deps args write-deps ret assertion)
  (let ([rel (hash-ref! relations-cache f (thunk (fresh-relation f)))])
    (apply expression `(, @app ,rel ,@read-deps ,@args ,@write-deps ,ret ,assertion))))

(define (argument-types->relation-type read-dependencies domain write-dependencies range)
  (if (and (empty? read-dependencies)
           (empty? domain)
           (empty? write-dependencies)
           (empty? range))
      @boolean?
      (apply ~> `(,@read-dependencies ,@domain ,@write-dependencies ,@range , @boolean?))))
