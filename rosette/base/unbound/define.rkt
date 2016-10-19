#lang racket

(require
  (for-syntax racket syntax/parse)
  (only-in "../core/term.rkt" constant constant? solvable-domain term-cache type-of)
  (only-in "../form/define.rkt" define-symbolic)
  "mutations.rkt"
  "rules.rkt")

(provide define/unbound rules->assertions)

(define-syntax (define/unbound stx)
  (syntax-parse stx
    [(_ (head args ...) type body body-rest ...)
     (quasisyntax/loc stx
       (define head
         (let*-values ([(head) (constant #'head type)]
                       [(arg-constants) (box #f)]
                       [(state/before) (create-rollback-point)]
                       [(term-cache-snapshot) (box #f)]
                       [(impl) (λ ()
                                 (mutables:=symbolic!/track state/before)
                                 (register-solvable-function head)
                                 #,@(for/list ([arg (syntax->list #'(args ...))]
                                               [i (in-naturals)])
                                      #`(define-symbolic #,arg (i-th-member-of-domain #,i type)))
                                 (set-box! arg-constants (list args ...))
                                 (set-box! term-cache-snapshot (hash-copy (term-cache)))
                                 body
                                 body-rest ...)]
                       [(term state/after) (speculate* (impl))]
                       [(scoped-constants) (hash-values-diff constant? (term-cache) (unbox term-cache-snapshot))]) ; TODO: make it more effective with splicing-let
           (eval/horn term
                      head
                      (unbox arg-constants)
                      (set-subtract scoped-constants (list->set (unbox arg-constants)))
                      state/after)
           (λ (args ...)
             (let ([constants (mutables:=symbolic!/memorize state/after)])
               ;(dbg "Resulting constants: ~a" constants)
               (function-application->symbolic-constant head (list args ...) constants))))))]))

(define (i-th-member-of-domain i type)
  (let ([domain (solvable-domain type)])
    (if (< i (length domain))
        (list-ref domain i)
        (error 'define "Too many arguments!"))))

(define (hash-values-diff filter-func hash1 hash2)
  (let ([values1 (apply set (filter filter-func (hash-values hash1)))]
        [values2 (apply set (filter filter-func (hash-values hash2)))])
    (set-subtract values1 values2)))
