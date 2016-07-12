; This file added temporary and will be filtered out from Git history

#lang rosette

(require "trace.rkt")

(require racket/file)

(current-bitwidth #f)
(define-symbolic x y integer?)
(define env (verify (assert (>= (+ x y) (- x y)))))
(format "x=~a" (evaluate x env))
(format "y=~a" (evaluate y env))

(show-file-trace "z3Trace")
