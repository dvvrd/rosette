#lang racket

(require
  (for-syntax racket/syntax)
  "function.rkt"
  (only-in "../core/bool.rkt" @boolean?)
  (only-in "../core/function.rkt" ~>)
  (only-in "contracts.rkt" define/typed lambda/typed λ/typed λtype λtyped?)
  (only-in "utils.rkt" gensym))

(provide define/unbound define/unbound/higher
         lambda/unbound λ/unbound
         define/typed lambda/typed λ/typed
         define/predicate lambda/predicate λ/predicate
         λtype λtyped?)

; Declares new function that will be encoded into a system of Horn clauses and solved by Horn solver.
; No actual body invocation will be performed when unbound function is called. Instead fresh symbolic
; constant will be returned. Also all variables that can mutate in any execution branch of unbound
; function will be "symbolized" (their values will be set to fresh symbolic contants).
; The body of a function will be executed in speculated environment when function is called for a first
; time. The body will be executed once if function is non-recursive or mutually recursive and twice
; otherwise.
(define-syntax (define/unbound stx)
  (syntax-case stx ()
    [(_ (head args ...) type body body-rest ...)
     (quasisyntax/loc stx
       (define head
         #,(make-solvable-function #'#'head
                                   #'(args ...)
                                   #'type
                                   #'(thunk body body-rest ...))))]
    [(_ head (functions ...) (args ...) type body body-rest ...)
     (with-syntax ([head/higher (format-id stx "~a/higher" #'head)])
       (let ([result
       (quasisyntax/loc stx
         (define (head functions ... args ...)
           (define/unbound/higher head/higher (functions ...) (args ...) type body body-rest ...)
           (head/higher args ...)))
 ]) (printf "Transformed to ~a\n" result) result)
       )]))

; define/unbound/higher acts like define/unbound, but instantiates horn-clauses for each new set
; of functions passed as parameters. Those functions should not be contained in args, instead
; they should be passed separately (just before normal arguments specification).
; In case when functions are not specified instantiates new clauses each time when called.
(define-syntax (define/unbound/higher stx)
  (syntax-case stx ()
    [(_ head (functions ...) (args ...) type body body-rest ...)
     (quasisyntax/loc stx
       (define head
         (instantiate-high-order-solvable-function
          'head
          (list functions ...)
          (thunk
           #,(make-solvable-function #'(datum->syntax #f (gensym (syntax->datum #'head)))
                                     #'(args ...)
                                     #'type
                                     #'(thunk body body-rest ...))))))]))

; Like define/unbound, but returns a lambda-expression.
(define-syntax (lambda/unbound stx)
  (syntax-case stx ()
    [(_ (args ...)  type body body-rest ...)
     (quasisyntax/loc stx
       #,(make-solvable-function #'(datum->syntax #f (gensym (syntax->datum #'λ)))
                                 #'(args ...)
                                 #'type
                                 #'(thunk body body-rest ...)))]))

; Shorthand for lambda/unbound.
(define-syntax λ/unbound (make-rename-transformer #'lambda/unbound))

; Like define/typed, but only argument types are specified. The range will be always boolean?.
(define-syntax-rule (define/predicate (head args ...) (arg-types ...) body body-rest ...)
  (define/typed (head args ...) (~> arg-types ... @boolean?)
    body
    body-rest ...))

; Like lambda/typed, but only argument types are specified. The range will be always boolean?.
(define-syntax-rule (lambda/predicate (args ...) (arg-types ...) body body-rest ...)
  (lambda/typed (args ...) (~> arg-types ... @boolean?)
    body
    body-rest ...))

; Shorthand for lambda/predicate.
(define-syntax λ/predicate (make-rename-transformer #'lambda/predicate))
