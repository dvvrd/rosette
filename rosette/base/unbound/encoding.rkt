#lang racket

(require
  racket/syntax
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
  "dependencies.rkt" "relation.rkt" "horn.rkt" )

(provide @app
         (rename-out [eval-body/horn eval/horn])
         function-application->symbolic-constant term->rules
         rules->assertions bound-var? relation?)


;; ----------------- Caches and parameters ----------------- ;;

(define current-head (make-parameter #f))
(define current-args (make-parameter #f))
(define current-premises (make-parameter (set)))
(define current-rules (make-parameter (list)))
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
    (current-rules (append (current-rules) resulting-clauses))))

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
  (parameterize ([current-rules (list)])
    (eval/horn t)
    (current-rules)))

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

(define (terms->horn-clauses ts)
  (map (curry term->horn-clauses #f) ts))

(define (function-application->symbolic-constant f args read-deps mutations)
  (let* ([id (gensym 'ε)]
         [ε (constant id (solvable-range (type-of f)))]
         [auto-premise (function-application->relation f read-deps args mutations ε)])
    (register-auto-constants (cons ε mutations) auto-premise)
    ε))

(define (rules->assertions clauses additional-premises)
   (clauses->assertions (append (current-rules) clauses) additional-premises))
