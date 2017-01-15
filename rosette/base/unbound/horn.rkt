#lang racket

(require
  racket/generic
  (only-in "../core/bool.rkt" @=> @&& @boolean?)
  (only-in "../core/term.rkt" constant constant? expression type-of term<?)
  (only-in "bound-vars.rkt" share-vars)
  (only-in "utils.rkt" substitute/constants))

(provide (struct-out horn-clause) clauses->assertions replace/clause
         dbg-level gen:horn-transformer register-horn-transformer)

(struct horn-clause (premises conclusion)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf
      port "~a"
      (cond
        [(set-empty? (horn-clause-premises self))
         (horn-clause-conclusion self)]
        [else
         (expression @=>
                     (if (= 1 (set-count (horn-clause-premises self)))
                         (set-first (horn-clause-premises self))
                         (apply expression @&& (set->list (horn-clause-premises self))))
                     (horn-clause-conclusion self))])))])

;; ----------------- Processing before passing to solver ----------------- ;;

(define-generics horn-transformer
  [pre-process horn-transformer clauses]
  [post-process horn-transformer terms])

(define dbg-level (make-parameter 0))
(define horn-transformers (make-parameter '()))

(define (register-horn-transformer transformer)
  (unless (horn-transformer? transformer)
    (raise-arguments-error register-horn-transformer "expected a Horn clauses transformer" "transformer" transformer))
  (horn-transformers (cons transformer (horn-transformers))))

(define (do-pre-processing clauses)
  (for ([transformer (horn-transformers)])
    (pre-process transformer clauses)))

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
       (when (and result (positive? (dbg-level))) (printf "Rule: ~a\n" clause))
       result)]
    [else clause]))

(define (clauses->assertions clauses)
  (do-pre-processing clauses)
  (share-vars
   (do-post-processing
    (filter identity
            (apply append
                   `(,@(for/list ([(head cs) (in-hash clauses)]
                                  #:when head)
                         (map clause->assertion cs))
                     ,(map clause->assertion (hash-ref clauses #f))))))))

; Performs a given substitution into all terms of clause.
(define (replace/clause subst clause)
  (horn-clause (list->set (set-map (horn-clause-premises clause) (curry substitute/constants subst)))
               (substitute/constants subst (horn-clause-conclusion clause))))
