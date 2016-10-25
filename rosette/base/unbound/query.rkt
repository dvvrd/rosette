#lang racket

(require
  "rules.rkt"
  "../../query/finitize.rkt"
  "../../solver/solver.rkt" "../../solver/horn-solver.rkt"
  (only-in "../../query/core.rkt" eval/asserts)
  (only-in "../core/bool.rkt" ! || @boolean? @&& @=>)
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
     (let*-values ([(fail-rel) (constant 'fail° @boolean?)]
                   [(premises) (eval/asserts (thunk pre))]
                   [(conclusions) (eval/asserts (thunk post))]
                   [(premises-vars premises) (premises-union premises)]
                   [(queries) (conclusions-union conclusions fail-rel)]
                   [(rules) (append (rules->assertions premises-vars premises) (rules->assertions queries premises-vars premises))])
       ;(dbg "FINAL RULES:\n~a" rules)
       (∃-solve rules fail-rel))]
    [(_ #:guarantee post) (verify/unbound #:assume #t #:guarantee post)]
    [(_ post) (verify/unbound #:assume #t #:guarantee post)]))

(define (premises-union assertions)
  (let* ([clauses (apply append (map term->rules assertions))]
         [bound-vars (apply set-union (cons (set) (map horn-clause-bound-vars clauses)))]
         [premises (apply set-union (cons (set) (map horn-clause-premises clauses)))]
         [conclusions (map horn-clause-conclusion clauses)])
    (values bound-vars (set-union premises (list->set conclusions)))))

(define (conclusions-union conclusions query)
  (let ([conclusions-clauses (apply append (map term->rules conclusions))])
    (map (λ (conclusion)
           (horn-clause
            (horn-clause-bound-vars conclusion)
            (set-add (horn-clause-premises conclusion) (! (horn-clause-conclusion conclusion)))
            query))
         conclusions-clauses)))

(define (map-fold proc init lst)
  (match lst
    ['() (values init '())]
    [`(,h . ,t) (let*-values ([(head-acc head) (proc h init)]
                              [(acc tail) (map-fold proc head-acc t)])
                    (values acc (cons head tail)))]))

(define (finitize-rule bw rule fmap)
  (match rule
    [(expression (== @=>) (expression (== @&&) premises ...) conclusion)
     (let* ([fmap (finitize (cons conclusion premises) bw fmap)]
            [fpremises (map (curry hash-ref fmap) premises)]
            [fconclusion (hash-ref fmap conclusion)])
       (values fmap (expression @=> (apply expression `(, @&& ,@fpremises)) fconclusion)))]
    [(expression (== @=>) premise conclusion)
     (let* ([fmap (finitize (list premise conclusion) bw fmap)]
            [fpremise (hash-ref fmap premise)]
            [fconclusion (hash-ref fmap conclusion)])
       (values fmap (expression @=> fpremise fconclusion)))]
    [_
     (let ([fmap (finitize (list rule) bw fmap)])
       (values fmap (hash-ref fmap rule)))]))

; Simply calling (finitize rules) breaks rules structure, so we deconstruct rules, finitize and construct back.
(define (finitize-rules rules bw)
  (map-fold (curry finitize-rule bw) (make-hash) rules))

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
           (let-values ([(fmap rules) (finitize-rules rules bw)])
             (solver-add-rules solver rules)
             ; TODO: Rosette engine generates multiple requests to solver to
             ; fit racket integer semantics. What to do here?
             (let ([fsol (complete (solver-query solver query) fmap)])
               (unfinitize fsol fmap))))]
        [else
         (solver-add-rules solver rules)
         (solver-query solver query)]))
    (solver-clear solver)))
