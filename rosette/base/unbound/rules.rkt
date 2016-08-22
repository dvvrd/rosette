#lang racket

(provide dbg horn-clause horn-clause? horn-clause-id horn-clause-premises horn-clause-conclusion
         add-rule add-rules rules->assertions)

(define-syntax-rule (dbg ft args ...)
  (displayln (format ft (if (syntax? args) (syntax->datum args) args) ...) (current-error-port)))
;  void)

(struct horn-clause (id premises conclusion)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a: [=> ~a ~a]" (syntax->datum (horn-clause-id self)) (map syntax->datum (horn-clause-premises self)) (syntax->datum (horn-clause-conclusion self))))])

(define rules (make-hash))

(define (add-rule id premises conclusion)
  (let* ([existing-clauses (hash-ref! rules id (list))]
         [id-val (id)]
         [premises-val (premises)]
         [conclusion-val (conclusion)]
         [current-clause (horn-clause id-val premises-val conclusion-val)])
        ;[current-clause #`(=> (and #,@(flatten premises)) #,conclusion)])
    (hash-set! rules id (cons current-clause existing-clauses))))

(define (add-rules clauses)
  (for-each add-rule clauses))

(define-syntax (rules->assertions stx)
  (syntax/loc stx (dbg "rules->assertions: rules: ~a" rules)))
