#lang racket

(require
  racket/generic
  "dependencies.rkt"
  (only-in "../core/term.rkt" term-type)
  (only-in "utils.rkt" term->constants terms->constants))

(provide gen:implicitly-constrained
         register-auto-constant register-auto-constants
         auto-premises term->constants/with-auto-premises)

(define-generics implicitly-constrained
  [implicit-constraints implicitly-constrained instance])

(define (get-implicit-constraints constant)
  (if (implicitly-constrained? (term-type constant))
      (implicit-constraints (term-type constant) constant)
      (set)))

(define auto-constants (make-hash))

(define (auto-premises constant)
  (cond
    [(hash-has-key? auto-constants constant)
     (cdr (hash-ref auto-constants constant))]
    [(implicitly-constrained? (term-type constant))
     (register-auto-constants* (cons constant (implicit-dependencies constant))
                               (get-implicit-constraints constant))
     (cdr (hash-ref auto-constants constant))]
    [else (set)]))

(define (register-auto-constants* constants auto-premises)
  (let* ([term-bound-vars (terms->constants (set->list auto-premises))]
         [auto-bound-vars-deps
          (apply set-union
                 (for/list ([v term-bound-vars])
                   (if (hash-has-key? auto-constants v)
                       (car (hash-ref auto-constants v ))
                       (set))))]
         [all-bound-vars (set-union term-bound-vars auto-bound-vars-deps)]
         [auto-premises-deps (list->set
                             (apply set-union
                                    (for/list ([v all-bound-vars])
                                      (if (hash-has-key? auto-constants v)
                                          (cdr (hash-ref auto-constants v))
                                          (get-implicit-constraints v)))))]
         [auto-premises (apply set-union (cons (set-union auto-premises-deps auto-premises) (map get-implicit-constraints constants)))]
         [auto-value (cons all-bound-vars auto-premises)])
    (for ([c constants])
      (hash-set! auto-constants c auto-value)
      (for ([d (implicit-dependencies c)])
        (hash-set! auto-constants d auto-value)))))

(define (register-auto-constants constants auto-premises)
  (register-auto-constants* constants (set auto-premises)))

(define (register-auto-constant constant auto-premise)
  (register-auto-constants (list constant) auto-premise))

(define (term->constants/with-auto-premises t)
  (let ([first-level-constants (term->constants t)])
    (apply set-union
           (cons first-level-constants
                 (for/list ([const first-level-constants])
                   (if (hash-has-key? auto-constants const)
                       (car (hash-ref auto-constants const))
                       (set)))))))
