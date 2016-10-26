#lang racket

(require (only-in "../core/effects.rkt" speculate/unsafe speculate* location-final-value location-current-value location=?)
         (only-in "../core/term.rkt" constant constant? type-of solvable? define-operator))

(provide state-union mutables:=symbolic!/track mutables:=symbolic!/memorize
         state->mutations state->current-values symbolization->actual-value
         speculate* create-rollback-point restore-symbolization)

(define create-rollback-point (speculate/unsafe))

(define tracked-values (make-hash))
(define memorized-values (make-hash))

(define (mutable:=symbolic!/report box pre post)
  (set-box! box (mutable:=symbolic! pre post))
  (unbox box))

(define (mutable:=symbolic! pre post)
  (let ([type (type-of pre)])
    (if (solvable? type)
        (constant (gensym 'μ) type)
        post)))

(define (state-union s1 s2)
  (remove-duplicates (append s1 s2) location=?))

; Modifies contents of solvable mutable variables in a given state
; writing into it fresh symbolic constant of corresponding type.
;
; In case when state was obtained by speculate* or create-rollback-point call
; starts tracking generated symbolic constants. For those constants
; symbolization->actual-value will return current (i.e. actual at the moment
; of call symbolization->actual-value) values of locations that were
; overwritten by tracked constants.
(define (mutables:=symbolic!/track state)
  (if (list? state)
      (for ([s state])
        (let ([b (box #f)])
          (s (curry mutable:=symbolic!/report b))
          (let ([new-val (unbox b)])
            (when (constant? new-val)
              (hash-set! tracked-values new-val (λ () (location-current-value s)))))))
      (state mutable:=symbolic!)))

; Modifies contents of solvable mutable variables in a given state
; writing into it fresh symbolic constant of corresponding type.
;
; In case when state was obtained by speculate* or create-rollback-point call
; returns a list containing generated symbolic constants in order
; they were generated. Also memorizes mapping of those constants to values
; that were overwritten by those constants, later those values can be obtained
; via value-before-symbolization.
(define (mutables:=symbolic!/memorize state)
  (if (list? state)
      (filter constant?
              (for/list ([s state])
                (let ([b (box #f)]
                      [old-val (location-current-value s)])
                  (s (curry mutable:=symbolic!/report b))
                  (let ([new-val (unbox b)])
                    (when (constant? new-val)
                      (hash-set! memorized-values new-val old-val))
                    new-val))))
      (state mutable:=symbolic!)))

; Returns a values of state after it was mutated (and before rolled-back).
(define (state->mutations state)
  (for/list ([s state])
    (location-final-value s)))

; Returns a value currently stored by a locations of the given state.
(define (state->current-values state)
  (for/list ([s state])
    (location-current-value s)))

; Returns a relevant value for the given symbolic constant.
; It may be value ovewritten by mutables:=symbolic/memorize or current value
; of location updated by mutables:=symbolic/track.
; If constant was created in other way (or even it is just some other value)
; then symbolization->actual-value returns it back.
(define (symbolization->actual-value constant)
  (hash-ref memorized-values constant
            (thunk
             (if (hash-has-key? tracked-values constant)
                 (let ([val ((hash-ref tracked-values constant))])
                   (hash-ref memorized-values val val))
                 constant))))

(define (restore-symbolization state)
  (if (list? state)
      (for ([s state])
        (s (λ (pre post) post)))
      (state (λ (pre post) post))))

; Just for debug, shorter suffixes
(require racket/syntax)
(define (gensym base)
  (begin0
    (format-id #f "~a~a" base (current-suffix))
    (current-suffix
     (add1 (current-suffix)))))
(define current-suffix (make-parameter 0))
