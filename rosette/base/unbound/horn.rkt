#lang racket

(require
  racket/generic
  (only-in "../core/bool.rkt" @=> @&& @boolean?)
  (only-in "../core/term.rkt" constant constant? expression type-of)
  (only-in "bound-vars.rkt" share-vars))

(provide (struct-out horn-clause) clauses->assertions
         gen:horn-transformer register-horn-transformer)

(struct horn-clause (premises conclusion)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port
              "[=> ~a ~a]"
              (cons '&& (set->list (horn-clause-premises self)))
              (horn-clause-conclusion self)))])

;; ----------------- Processing before passing to solver ----------------- ;;

(define-generics horn-transformer
  [pre-process horn-transformer clauses]
  [post-process horn-transformer terms])

(define horn-transformers (make-parameter '()))

(define (register-horn-transformer transformer)
  (unless (horn-transformer? transformer)
    (raise-arguments-error register-horn-transformer "expected a Horn clauses transformer" "transformer" transformer))
  (horn-transformers (cons transformer (horn-transformers))))

(define (do-pre-processing clauses)
  (foldl pre-process clauses (horn-transformers)))

(define (do-post-processing clauses)
  (foldl post-process clauses (horn-transformers)))

;; ----------------- Translation to symbolic expression ----------------- ;;

(define (horn-clause->implication clause)
  (match (set->list (horn-clause-premises clause))
    [(list) (horn-clause-conclusion clause)]
    [(list premise) (apply expression @=> (list premise (horn-clause-conclusion clause)))]
    [_ (let ([premise (apply @&& (set->list (horn-clause-premises clause)))])
         (and premise (expression @=> premise (horn-clause-conclusion clause))))]))

(define (clause->assertion clause)
  (cond
    [(horn-clause? clause)
     (let ([result (horn-clause->implication clause)])
       (when result (printf "Rule: ~a\n" clause))
       result)]
    [else clause]))

(define (enrich clauses additional-premises)
  (for/list ([clause clauses])
    (horn-clause (set-union additional-premises (horn-clause-premises clause))
                 (horn-clause-conclusion clause))))

(define (clauses->assertions clauses additional-premises)
  (share-vars
   (do-post-processing
    (filter identity
            (map clause->assertion
                 (do-pre-processing
                  (enrich clauses additional-premises)))))))
