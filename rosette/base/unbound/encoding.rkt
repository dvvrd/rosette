#lang racket

(require
  racket/syntax
  (only-in "../core/bool.rkt" && || ! @&& @|| @!)
  (only-in "../core/term.rkt"
           constant constant? expression @app
           type-of solvable-domain solvable-range type-applicable?)
  (only-in "../core/polymorphic.rkt"
           ite ite* guarded)
  (only-in "../core/bool.rkt" @boolean? @!)
  (only-in "bound-vars.rkt" bound-var?)
  (only-in "mutations.rkt" state->mutations)
  (only-in "utils.rkt" for**/list gensym)
  (only-in "auto-constants.rkt" auto-premises register-auto-constants)
  "dependencies.rkt" "relation.rkt" "horn.rkt")

(provide @app
         (rename-out [eval-body/horn eval/horn])
         function-application->symbolic-constant
         premises->assertion-constants
         term->rules rules->assertions bound-var? relation?
         ite-compactification)


;; ----------------- Caches and parameters ----------------- ;;

(define current-head (make-parameter #f))
(define current-args (make-parameter #f))
(define current-rules (make-parameter (make-hash)))
(define current-mutations-clauses (make-parameter #f))
(define current-assertion-clauses (make-parameter #f))

(define (add-rules terms-and-guards)
  (for ([term-and-guards terms-and-guards])
    (add-rule term-and-guards)))

; Takes conclusion and constructs horn-clauses using current traversal context.
(define (add-rule term-and-guards)
  (let* ([value (car term-and-guards)]
         [premises (cdr term-and-guards)]
         [resulting-clauses
          (if (current-head)
              (apply append
               (for**/list
                (current-assertion-clauses)
                (λ (assertions-clauses)
                  (for**/list
                   (current-mutations-clauses)
                   (λ (mutations-clauses)
                     (let* ([additional-premises
                             (if (and (null? assertions-clauses)
                                      (null? mutations-clauses))
                                 (set)
                                 (apply set-union (append
                                                   (map horn-clause-premises assertions-clauses)
                                                   (map horn-clause-premises mutations-clauses))))]
                            [premises (set-union premises additional-premises)])
                       (horn-clause premises
                                    (function-application->relation (current-head)
                                                                    (read-dependencies/original (current-head))
                                                                    (current-args)
                                                                    (map horn-clause-conclusion mutations-clauses)
                                                                    value
                                                                    (apply @&&
                                                                           (append (map horn-clause-conclusion assertions-clauses)
                                                                                   (premises->assertion-constants premises)))))))))))
              (list (horn-clause premises value)))])
    (hash-update! (current-rules) (function->relation (current-head))
                  (λ (rules) (append rules resulting-clauses))
                  (list))))

;; ----------------- Symbolic term --> Horn clauses ----------------- ;;

(define (eval-body/horn t head args assertions)
  (parameterize ([current-head head]
                 [current-args args])
    (define-values (mutations-clauses assertion-clauses)
      (parameterize ([current-head #f]
                     [current-args #f])
        (values
         (for/list ([m (write-dependencies head)])
           (term->rules m))
         (for/list ([a assertions])
           (term->rules a)))))

    (parameterize ([current-mutations-clauses mutations-clauses]
                   [current-assertion-clauses assertion-clauses])
      (eval/horn t))))

(define (eval/horn t)
  (add-rules (term->horn-clauses #t t)))

(define (term->rules t)
  (parameterize ([current-rules (make-hash)])
    (eval/horn t)
    (hash-ref (current-rules) #f '())))

(define (compose-guards conds ps [not? #f])
  (apply append
         (map (λ (p)
                (map (λ (cond)
                       (cons (car p)
                             (let ([guard (if not? (@! (car cond)) (car cond))])
                               (set-union (if (equal? guard #t) (set) (set guard))
                                          (cdr cond)
                                          (cdr p)))))
                     conds))
              ps)))

(define ite-compactification (make-parameter #f))

(define (term->horn-clauses tail-position? t)
  (match t
    [(expression (== dependent-app) (expression (== @app) f args ...) read-deps mutations)
     (for**/list (terms->horn-clauses args)
                 (λ (args)
                   (let ([result
                          (function-application->symbolic-constant f
                                                                   (map car args)
                                                                   read-deps
                                                                   mutations)])
                     (cons result
                           (apply set-union
                                  (cons (auto-premises result)
                                        (map cdr args)))))))]
    [(expression (== ite) test then else)
     #:when (or tail-position? (not (ite-compactification)))
     (let ([test (term->horn-clauses #f test)]
           [then (term->horn-clauses tail-position? then)]
           [else (term->horn-clauses tail-position? else)])
       (append (compose-guards test then)
               (compose-guards test else #t)))]
    [(expression (== ite*) gvs)
     #:when (or tail-position? (not (ite-compactification)))
     (apply append
            (map (λ (gv)
                   (match-let* ([(guarded g v) gv]
                                [g (term->horn-clauses #f g)])
                     (compose-guards g (term->horn-clauses tail-position? v))))
                 gvs))]
    [(or (expression (== @||) (expression (== @&&) c t) (expression (== @&&) (expression (== @!) c) e))
         (expression (== @||) (expression (== @&&) c t) (expression (== @&&) e (expression (== @!) c)))
         (expression (== @||) (expression (== @&&) (expression (== @!) c) e) (expression (== @&&) c t))
         (expression (== @||) (expression (== @&&) (expression (== @!) c) e) (expression (== @&&) t c))
         (expression (== @||) (expression (== @&&) t c) (expression (== @&&) (expression (== @!) c) e))
         (expression (== @||) (expression (== @&&) t c) (expression (== @&&) e (expression (== @!) c)))
         (expression (== @||) (expression (== @&&) e (expression (== @!) c)) (expression (== @&&) c t))
         (expression (== @||) (expression (== @&&) e (expression (== @!) c)) (expression (== @&&) t c)))
     (term->horn-clauses tail-position? (expression ite c t e))]
    [(expression op args ...)
     (for**/list (terms->horn-clauses args)
                 (λ (args)
                   (cons (apply expression `(,op ,@(map car args)))
                         (apply set-union (map cdr args)))))]
    [(constant _ _)
     (list (cons t (auto-premises t)))]
    [_ (list (cons t (set)))]))

(define (terms->horn-clauses ts)
  (map (curry term->horn-clauses #f) ts))

(define (eliminate-dependent-apps t)
  (match t
    [(expression (== dependent-app) (expression (== @app) f args ...) read-deps mutations)
     (eliminate-dependent-apps
      (function-application->symbolic-constant f (eliminate-dependent-apps args) read-deps mutations))]
    [(expression op args ...)
     (let ([new-args (eliminate-dependent-apps args)])
       (if (equal? new-args args)
           t
           (apply expression `(,op ,@new-args))))]
    [(list xs ...) (map eliminate-dependent-apps xs)]
    [_ t]))

(define (function-application->symbolic-constant f args read-deps mutations)
  (let* ([result-id (gensym 'ε)]
         [ε (constant result-id (solvable-range (type-of f)))]
         [α (constant (gensym 'α) @boolean?)]
         [auto-premise (function-application->relation f read-deps args mutations ε α)])
    (register-auto-constants (cons ε mutations) (eliminate-dependent-apps auto-premise))
    ε))

(define (premises->assertion-constants premises)
  (filter identity
          (set-map premises relation-application->assertion-constant)))

(define (relation-application->assertion-constant expr)
  (match expr
    [(expression @app f args ...) #:when (relation? f) (last args)]
    [_ #f]))

(define (rules->assertions clauses)
  (define copy (hash-copy (current-rules)))
  (hash-set! copy #f clauses)
  (clauses->assertions copy))
