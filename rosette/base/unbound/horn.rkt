#lang racket

(require
  racket/syntax
  (only-in "../core/bool.rkt" @=> @&& @boolean?)
  (only-in "../core/term.rkt" constant constant? expression type-of)
  (only-in "relation.rkt" relation?)
  (only-in "bound-vars.rkt" share-vars))

(provide (struct-out horn-clause) clauses->assertions)

(struct horn-clause (bound-vars premises conclusion)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port
              "∀(~a) [=> ~a ~a]"
              (string-join
               (set-map
                (horn-clause-bound-vars self)
                (curry format "~a"))
               ", ")
              (cons '&& (set->list (horn-clause-premises self)))
              (horn-clause-conclusion self)))])


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
     (let ([result (share-vars (horn-clause-bound-vars clause)
                               (horn-clause->implication clause))])
       (when result (printf "Rule: ~a\n" clause))
       result)]
    [else clause]))

(define (enrich clauses additional-bound-vars additional-premises)
  (for/list ([clause clauses])
    (horn-clause (set-union additional-bound-vars (horn-clause-bound-vars clause))
                 (set-union additional-premises (horn-clause-premises clause))
                 (horn-clause-conclusion clause))))

(define (clauses->assertions clauses additional-bound-vars additional-premises)
  (filter identity
          (map clause->assertion
               (enrich clauses additional-bound-vars additional-premises))))
