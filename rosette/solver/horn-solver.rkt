#lang racket

(require racket/generic)

(provide gen:horn-solver horn-solver?
         solver-add-rules solver-query)

; The generic Horn solver interface specifies the set of procedures that 
; should be provided by a rosette/unbound solver. Horn solvers work with
; datalog input format.
;
; The solver-add-rules procedure takes as input zero or more @rule 
; values and uses them to infer invariants about the queried relations.
; The solver-check procedure tries to justify the queried relation.
; If the rules are satisfiable, the resulting solution is sat?,
; otherwise it is unsat?.
(define-generics horn-solver
  [solver-add-rules horn-solver rules]
  [solver-query horn-solver query])
