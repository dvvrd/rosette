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
         function-application->symbolic-constant term->rules
         rules->assertions bound-var? relation?)


;; ----------------- Caches and parameters ----------------- ;;

(define current-head (make-parameter #f))
(define current-args (make-parameter #f))
(define current-premises (make-parameter (set)))
(define current-rules (make-parameter (make-hash)))
(define current-mutations-clauses (make-parameter #f))

(define (add-premise premise)
  (current-premises (set-add (current-premises) premise)))

(define (add-premises premises)
  (unless (set-empty? premises)
    (current-premises (set-union (current-premises) premises))))

; Takes conclusion and constructs horn-clauses using current traversal context.
(define (add-rule value)
  (let* ([resulting-clauses
          (if (current-head)
              (for**/list (current-mutations-clauses)
                          (λ (clauses)
                            (let ([additional-premises (if (null? clauses)
                                                           (set)
                                                           (apply set-union (map horn-clause-premises clauses)))])
                              (horn-clause (set-union (current-premises) additional-premises)
                                           (function-application->relation (current-head)
                                                                           (read-dependencies/original (current-head))
                                                                           (current-args)
                                                                           (map horn-clause-conclusion clauses)
                                                                           value)))))
              (list (horn-clause (current-premises) value)))])
    (hash-update! (current-rules) (function->relation (current-head))
                  (λ (rules) (append rules resulting-clauses))
                  (list))))

;; ----------------- Symbolic term --> Horn clauses ----------------- ;;

(define (eval-body/horn t head args)
  (parameterize ([current-head head]
                 [current-args args])
    (define mutations-clauses
      (parameterize ([current-head #f]
                     [current-args #f])
        (for/list ([m (write-dependencies head)])
          (term->rules m))))

    (parameterize ([current-mutations-clauses mutations-clauses])
      (eval/horn t))))

(define (eval/horn t)
  (parameterize ([current-premises (current-premises)])
    (term->horn-clauses #t t)))

(define (term->rules t)
  (parameterize ([current-rules (make-hash)])
    (eval/horn t)
    (hash-ref (current-rules) #f '())))

(define (term->horn-clauses tail-position? t)
  (match t
    [(expression (== @app) f args ...)
     (let* ([args (terms->horn-clauses args)]
            [result (expression @app f args)])
       (when tail-position? (add-rule result))
       result)]
    [(expression (== dependent-app) (expression (== @app) f args ...) read-deps mutations)
     (term->horn-clauses tail-position?
                         (function-application->symbolic-constant f (terms->horn-clauses args) read-deps mutations))]
    [(expression (== ite) test then else) #:when tail-position?
     (let* ([test (term->horn-clauses #f test)]
            [then
             (parameterize ([current-premises (set-add (current-premises) test )])
               (term->horn-clauses tail-position? then))]
            [else
             (parameterize ([current-premises (set-add (current-premises) (@! test))])
               (term->horn-clauses tail-position? else))])
       (expression ite test then else))]
    [(expression (== ite*) gvs) #:when tail-position?
     (expression ite*
                 (map (λ (gv)
                        (match-let* ([(guarded g v) gv]
                                     [g (term->horn-clauses #f g)])
                          (parameterize ([current-premises (set-add (current-premises) g)])
                            (term->horn-clauses tail-position? v))))
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
     (let* ([args (terms->horn-clauses args)]
            [result (apply expression `(,op ,@args))])
       (when tail-position? (add-rule result))
       result)]
    [(constant _ _)
     (add-premises (auto-premises t))
     (when tail-position? (add-rule t))
     t]
    [_
     (when tail-position? (add-rule t))
     t]))

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

(define (terms->horn-clauses ts)
  (map (curry term->horn-clauses #f) ts))

(define (function-application->symbolic-constant f args read-deps mutations)
  (let* ([id (gensym 'ε)]
         [ε (constant id (solvable-range (type-of f)))]
         [auto-premise (function-application->relation f read-deps args mutations ε)])
    (register-auto-constants (cons ε mutations) (eliminate-dependent-apps auto-premise))
    ε))

(define (rules->assertions clauses additional-premises)
  (define copy (hash-copy (current-rules)))
  (hash-set! copy #f clauses)
  (clauses->assertions copy additional-premises))
