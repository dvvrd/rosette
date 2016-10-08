#lang racket

(require
  "rules.rkt"
  "../../query/core.rkt"
  (only-in "../core/bool.rkt" ! ||)
  (only-in "../../query/form.rkt" solve current-solver)
  (only-in "../../solver/smt/spacer.rkt" spacer)
  (only-in "../core/term.rkt" constant expression @app))

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
     (let* ([premises (eval/asserts (thunk pre))]
            [conclusions (eval/asserts (thunk post))]
            [queries (assertions->horn-clause premises conclusions)]
            [assestions (append (rules->assertions) (rules->assertions queries))])
       (dbg "FINAL ASSERTS:\n~a" assestions)
       (∃-solve assestions))]
    [(_ #:guarantee post) (verify/unbound #:assume #t #:guarantee post)]
    [(_ post) (verify/unbound #:assume #t #:guarantee post)]))

(define (assertions->horn-clause premises conclusions)
  (let* ([premises (apply append (map term->rules premises))]
         [premises-free-vars (apply append (map horn-clause-free-vars premises))]
         [premises-premises (apply append (map horn-clause-premises premises))]
         [premises-conclusions (map horn-clause-conclusion premises)]
         [premises (append premises-premises premises-conclusions)]
         [conclusions-clauses (apply append (map term->rules conclusions))])
    (map (λ (conclusion)
           (horn-clause
            (append premises-free-vars (horn-clause-free-vars conclusion))
            (append premises (horn-clause-premises conclusion))
            (horn-clause-conclusion conclusion))) conclusions-clauses)))
