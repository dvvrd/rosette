#lang racket

(require racket/runtime-path "z3.rkt")

; Spacer solver is Horn solver for unbound model checking.
; It is a fork of Z3 and interaction with it is just the same,
; so simply providing the renamed z3 symbols.

(provide spacer (rename-out [z3? spacer?]))

(define-runtime-path spacer-path (build-path ".." ".." ".." "bin" "spacer"))
(define (spacer)
  (let ([real-spacer-path
         ;; Check for 'spacer' and 'spacer.exe' executables, else print a warning
         (if (file-exists? spacer-path)
             spacer-path
             (let ([spacer.exe-path (path-replace-suffix spacer-path ".exe")])
               (if (file-exists? spacer.exe-path)
                   spacer.exe-path
                   (begin
                     (printf "Warning: could not find spacer solver executable in '~a'. Using μZ"
                             (path->string (simplify-path (path->directory-path spacer.exe-path))))
                     ""))))])
    (if (empty? real-spacer-path) (z3) (z3 real-spacer-path))))
