#lang racket

(require
  racket/syntax
  (only-in "../core/term.rkt"
           constant constant? expression @app
           type-of solvable-domain solvable-range type-applicable?)
  (only-in "../core/polymorphic.rkt"
           ite ite* guarded)
  (only-in "../core/bool.rkt" @boolean? @!)
  (only-in "mutations.rkt" state->mutations)
  (only-in "utils.rkt" for**/list terms->constants term->constants)
  (only-in "bound-vars.rkt" bound-var?)
  "dependencies.rkt" "relation.rkt" "horn.rkt")

(provide @app
         (rename-out [eval-body/horn eval/horn])
         function-application->symbolic-constant term->rules
         rules->assertions bound-var? relation?)


;; ----------------- Caches and parameters ----------------- ;;

(define current-head (make-parameter #f))
(define current-args (make-parameter #f))
(define current-bound-vars (make-parameter (set)))
(define current-premises (make-parameter (set)))
(define current-rules (make-parameter (list)))
(define current-intermediate-vars-count (make-parameter 0))
(define current-mutations-clauses (make-parameter #f))

(define auto-constants (make-hash))

(define (add-bound-var var)
  (current-bound-vars (set-add (current-bound-vars) var)))

(define (add-bound-vars vars)
  (current-bound-vars (set-union (current-bound-vars) vars)))

(define (add-premise premise)
  (current-premises (set-add (current-premises) premise)))

(define (add-premises premises)
  (current-premises (set-union (current-premises) premises)))

; Takes conclusion and constructs horn-clauses using current traversal context.
(define (add-rule value)
  (let* ([resulting-clauses
          (if (current-head)
              (for**/list (current-mutations-clauses)
                          (λ (clauses)
                            (let ([additional-bound-vars (if (null? clauses)
                                                             (set)
                                                             (apply set-union (map horn-clause-bound-vars clauses)))]
                                  [additional-premises (if (null? clauses)
                                                           (set)
                                                           (apply set-union (map horn-clause-premises clauses)))])
                              (horn-clause (set-union (current-bound-vars) additional-bound-vars)
                                           (set-union (current-premises) additional-premises)
                                           (function-application->relation (current-head)
                                                                           (read-dependencies/original (current-head))
                                                                           (current-args)
                                                                           (map horn-clause-conclusion clauses)
                                                                           value)))))
              (list (horn-clause (current-bound-vars) (current-premises) value)))])
    (current-rules (append (current-rules) resulting-clauses))))

(define (fresh-intermediate-var prefix)
  (begin0
    (format-id #f "~a~a" prefix (current-intermediate-vars-count))
    (current-intermediate-vars-count
     (add1 (current-intermediate-vars-count)))))

;; ----------------- Symbolic term --> Horn clauses ----------------- ;;

(define (eval-body/horn t head args scope mutations)
  (parameterize ([current-head head]
                 [current-args args])
    (define mutations-clauses
      (parameterize ([current-head #f]
                     [current-args #f])
        (for/list ([m (state->mutations mutations)])
          (term->rules m))))

    (parameterize ([current-mutations-clauses mutations-clauses]
                   [current-bound-vars (current-bound-vars)])
      (add-bound-vars (list->set (current-args)))
      (add-bound-vars (list->set (read-dependencies/original (current-head))))
      (eval/horn t))))

(define (eval/horn t)
  (parameterize ([current-intermediate-vars-count (current-intermediate-vars-count)]
                 [current-bound-vars (current-bound-vars)]
                 [current-premises (current-premises)])
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
    [(expression (== ite) test then else)
     (let* ([test (term->horn-clauses #f test)]
            [then
             (parameterize ([current-premises (set-add (current-premises) test )])
               (term->horn-clauses tail-position? then))]
            [else
             (parameterize ([current-premises (set-add (current-premises) (@! test))])
               (term->horn-clauses tail-position? else))])
       (expression ite test then else))]
    [(expression (== ite*) gvs)
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
     (add-bound-var t)
     (when (hash-has-key? auto-constants t)
       (match-define (cons auto-bound-vars auto-premises) (hash-ref auto-constants t))
       (add-bound-vars auto-bound-vars)
       (add-premises auto-premises))
     (when tail-position? (add-rule t))
     t]
    [_
     (when tail-position? (add-rule t))
     t]))

(define (terms->horn-clauses ts)
  (map (curry term->horn-clauses #f) ts))

(define (function-application->symbolic-constant f args read-deps mutations)
  (let* ([id (fresh-intermediate-var 'ε)]
         [ε (constant id (solvable-range (type-of f)))]
         [write-deps mutations]
         [auto-premise (function-application->relation f read-deps args write-deps ε)]
         [auto-bound-vars (term->constants auto-premise)]
         [auto-bound-vars-deps (apply set-union
                                      (for/list ([v auto-bound-vars])
                                        (if (hash-has-key? auto-constants v)
                                            (car (hash-ref auto-constants v ))
                                            (set))))]
         [auto-bound-vars (set-union auto-bound-vars auto-bound-vars-deps)]
         [auto-premise-deps (list->set
                             (apply set-union
                                    (for/list ([v auto-bound-vars])
                                      (if (hash-has-key? auto-constants v)
                                          (cdr (hash-ref auto-constants v))
                                          (set)))))]
         [auto-premises (set-add auto-premise-deps auto-premise)]
         [auto-value (cons auto-bound-vars auto-premises)])
    (for ([m mutations])
      (hash-set! auto-constants m auto-value))
    (hash-set! auto-constants ε auto-value)
    ε))

(define-syntax rules->assertions
  (syntax-rules ()
    [(_ clauses additional-bound-vars additional-premises)
     (clauses->assertions clauses additional-bound-vars additional-premises)]
    [(_ additional-bound-vars additional-premises)
     (clauses->assertions (current-rules) additional-bound-vars additional-premises)]))
