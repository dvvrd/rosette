#lang racket

(require
  (for-syntax racket syntax/parse)
  (only-in "../core/term.rkt" constant solvable-domain)
  (only-in "../form/define.rkt" define-symbolic)
  "rules.rkt")

(provide define/unbound rules->assertions)

(define-syntax (define/unbound stx)
  (syntax-parse stx
    [(_ (head args ...) type body body-rest ...)
     (quasisyntax/loc stx
       (define head
         (let* ([head (constant #'head type)]
                [arg-constants (box #f)]
                [impl (λ ()
                        (register-solvable-function head)
                        #,@(for/list ([arg (syntax->list #'(args ...))]
                                      [i (in-naturals)])
                             #`(define-symbolic #,arg (i-th-member-of-domain #,i type)))
                        (set-box! arg-constants (list args ...))
                        body
                        body-rest ...)]
                [term (impl)]) ; TODO: speculate here?
           (parameterize ([current-head head]
                          [current-args (unbox arg-constants)])
             (eval/horn term))
           head)))]))

(define (i-th-member-of-domain i type)
  (let ([domain (solvable-domain type)])
    (if (< i (length domain))
        (list-ref domain i)
        (error 'define "Too many arguments!"))))
