#lang racket

(require (except-in "safe.rkt"
                    list list? null null? length car cdr
                    foldl map andmap ormap append
                    caar cadr cdar cddr
                    caaar caadr cadar caddr cdaar cdadr cddar cdddr
                    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                    first rest second third fourth fifth sixth seventh eighth ninth tenth
                    clear-state!)
         (only-in "safe.rkt" [clear-state! clear-state!/bound])
         "base/unbound/unbound.rkt")

(current-bitwidth #f)

(define (clear-state!)
  (clear-state!/bound)
  (current-bitwidth #f)
  (current-horn-solver (spacer)))

(provide
 (all-from-out
  racket
  "safe.rkt"
  "base/unbound/unbound.rkt")
 clear-state!)
