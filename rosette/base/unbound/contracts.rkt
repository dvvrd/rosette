#lang racket

(require
  racket/contract/region
  (only-in "../core/type.rkt" solvable-domain solvable-range type-applicable?))

(provide λtype λtyped?
         define/typed lambda/typed λ/typed)

(define (rosette-type->contract t)
  (match t
    [(? type-applicable?) (function-type->contract t)]
    [_ t]))

(define (function-type->contract t)
  (dynamic->* #:mandatory-domain-contracts (map rosette-type->contract (solvable-domain t))
              #:range-contracts (list (rosette-type->contract (solvable-range t)))))

(define-syntax (define/typed stx)
  (syntax-case stx ()
    [(_ (head args ...) type body body-rest ...)
     (quasisyntax/loc stx
       (define head
         (lambda/typed #:name head (args ...) type body body-rest ...)))]))

(define-syntax (lambda/typed stx)
  (syntax-case stx ()
    [(_ #:name name (args ...) type body body-rest ...)
     (quasisyntax/loc stx
       (let ()
         (define/contract (name args ...) (rosette-type->contract type)
           body
           body-rest ...)
         (hash-set! λtypes name type)
         name))]
    [(_ (args ...) type body body-rest ...)
     #`(lambda/typed #:name #,(gensym 'lambda) (args ...) type body body-rest ...)]))

(define-syntax λ/typed (make-rename-transformer #'lambda/typed))

(define λtypes (make-hasheq))

(define (λtyped? function)
  (hash-has-key? λtypes function))

(define (λtype function caller)
  (hash-ref λtypes function
            (thunk
             (error caller (string-append "~a has no type specification. "
                                          "Annotate type with define/typed, lambda/typed, "
                                          "define/unbound or lambda/unbound.") function))))
