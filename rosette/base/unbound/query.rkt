#lang racket

(require
  racket/syntax
  "../core/bool.rkt"
  "../../query/core.rkt"
  "../../query/form.rkt"
  "../../solver/smt/spacer.rkt"
  (only-in "../core/term.rkt" constant expression @app)
  (only-in "rules.rkt"
           horn-clause rules->assertions
           recursive? recursive-function->symbolic-relation
           symbolic-constant->symbolic-relation dbg)
  ; TODO: DELETE IT WHEN TYPES ARE IMPLEMENTED!
  (only-in "../core/real.rkt" @integer?)
  (only-in "../core/function.rkt" ~>))

(provide solve/unbound verify/unbound current-horn-solver)

(define current-horn-solver (make-parameter (spacer)))

; Same as usual rosette "solve", but uses smt fixedpoint engine for
; infering recursive functions invariants without their evaluation.
(define-syntax-rule (solve/unbound form forms ...)
  (parameterize ([current-solver (current-horn-solver)])
      (pass-rules-to-solver)
      (solve form forms ...)))

; Same as usual rosette "verify", but uses smt fixedpoint engine for
; infering recursive functions invariants without their evaluation.
(define-syntax verify/unbound
  (syntax-rules ()
    [(_ #:assume pre #:guarantee post)
     (parameterize ([current-intermediate-vars-count (current-intermediate-vars-count)])
       (let* ([premises (eval/asserts (thunk pre))]
              [conclusion (apply || (map ! (eval/asserts (thunk post))))]
              [query (assertions->horn-clause premises conclusion)]
              [ass (append (rules->assertions) (rules->assertions (list query)))])
         (dbg "FINAL ASSERTS:\n~a" ass)
         (∃-solve ass)))]
    [(_ #:guarantee post) (verify/unbound #:assume #t #:guarantee post)]
    [(_ post) (verify/unbound #:assume #t #:guarantee post)]))

(define (assertions->horn-clause premises conclusion)    
  (let-values ([(premises-free-vars premises-premises premises-terms) (terms->horn-clause-values premises)]
               [(conclusion-free-vars conclusion-premises conclusion-term) (term->horn-clause-values conclusion)])
    (horn-clause #'verify/unbound
                 (append premises-free-vars conclusion-free-vars)
                 (append premises-premises premises-terms conclusion-premises)
                 conclusion-term)))

  (define (term->horn-clause-values t)
    (match t
      [(expression (== @app) f args ...)
       (let-values ([(free-vars premises args) (terms->horn-clause-values args)])
         (if (recursive? f)
             (let* ([id (fresh-intermediate-var 'ρ)]
                    [rel (symbolic-constant->symbolic-relation f)] ; TODO: TYPES!
                    [ρ (constant id @integer?)]) ; TODO: TYPES!
               (values (cons ρ free-vars)
                       (cons (apply expression `(, @app ,rel ,@args ,ρ)) premises)
                       ρ))
             (values free-vars premises (expression @app f args))))]
      [(expression op args ...)
       (let-values ([(free-vars premises args) (terms->horn-clause-values args)])
         (values free-vars premises (apply op args)))]
      [(constant _ _) (values (list t) '() t)]
      [_ (values '() '() t)]))

  (define (terms->horn-clause-values ts)
    (let ([resulting-list
           (foldr
            (λ (t acc)
              (let-values ([(free-vars premises conclusion) (term->horn-clause-values t)])
                (list (append free-vars (first acc))
                      (append premises (second acc))
                      (cons conclusion (third acc)))))
            (list '() '() '()) ts)])
      (values (first resulting-list) (second resulting-list) (third resulting-list))))

(define current-intermediate-vars-count (make-parameter 0))

(define (fresh-intermediate-var prefix)
  (begin0
    (format-id #f "~a~a" prefix (current-intermediate-vars-count))
    (current-intermediate-vars-count
     (add1 (current-intermediate-vars-count)))))
