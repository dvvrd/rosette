#lang racket

(require
  "rules.rkt"
  "../../query/finitize.rkt"
  "../../solver/solver.rkt" "../../solver/horn-solver.rkt"
  (only-in "../../query/core.rkt" eval/asserts)
  (only-in "../core/bool.rkt" ! || @boolean?)
  (only-in "../../query/form.rkt" solve current-solver)
  (only-in "../../solver/smt/spacer.rkt" spacer)
  (only-in "../core/term.rkt" term-cache constant expression @app))

(provide solve/unbound verify/unbound current-horn-solver)

(define current-horn-solver (make-parameter (spacer)))

; Same as usual rosette "solve", but uses smt fixedpoint engine for
; infering recursive functions invariants without their evaluation.
(define-syntax-rule (solve/unbound form forms ...)
  (parameterize ([current-solver (current-horn-solver)])
      (error 'query "solve/unbound is not implemented yet...")))

; Same as usual rosette "verify", but uses smt fixedpoint engine for
; infering recursive functions invariants without their evaluation.
(define-syntax verify/unbound
  (syntax-rules ()
    [(_ #:assume pre #:guarantee post)
     (let* ([fail-rel (expression @rel (constant 'fail° @boolean?))]
            [premises (eval/asserts (thunk pre))]
            [conclusions (eval/asserts (thunk post))]
            [queries (assertions->horn-clause premises conclusions fail-rel)]
            [rules (append (rules->assertions) (rules->assertions queries))])
;       (dbg "FINAL RULES:\n~a" rules)
       (∃-solve rules fail-rel))]
    [(_ #:guarantee post) (verify/unbound #:assume #t #:guarantee post)]
    [(_ post) (verify/unbound #:assume #t #:guarantee post)]))

(define (assertions->horn-clause premises conclusions query)
  (let* ([premises (apply append (map term->rules premises))]
         [premises-bound-vars (apply set-union (cons (set) (map horn-clause-bound-vars premises)))]
         [premises-premises (apply append (map horn-clause-premises premises))]
         [premises-conclusions (map horn-clause-conclusion premises)]
         [premises (append premises-premises premises-conclusions)]
         [conclusions-clauses (apply append (map term->rules conclusions))])
    (map (λ (conclusion)
           (horn-clause
            (set-union premises-bound-vars (horn-clause-bound-vars conclusion))
            (append premises (horn-clause-premises conclusion) (list (! (horn-clause-conclusion conclusion))))
            query)) conclusions-clauses)))

; Searches for a model, if any, for the conjunction
; of the given formulas, using the provided solver and
; bitwidth. The solver and the bitwidth are, by default,
; current-horn-solver and current-bitwidth.  Returns
; sat or unsat and the generalized counter-example (if unsat).
; This procedure clears the solver's state before and after use.
(define (∃-solve rules query
                 #:solver [solver (current-horn-solver)]
                 #:bitwidth [bw (current-bitwidth)])
  (solver-clear solver)
  (begin0
    (with-handlers ([exn? (lambda (e) (solver-shutdown solver) (raise e))])
      (cond
        [bw
         (parameterize ([term-cache (hash-copy (term-cache))])
           (define fmap (finitize rules bw))
           (solver-add-rules solver (for/list ([rule rules]) (hash-ref fmap rule)))
             (let ([fsol (complete (solver-query solver query) fmap)])
               (unfinitize fsol fmap)))]
        [else
         (solver-add-rules solver rules)
         (solver-query solver query)]))
    (solver-clear solver)))
