#lang racket

(require racket/runtime-path
         "server.rkt" "cmd.rkt" "env.rkt"
         "../solver.rkt" "../horn-solver.rkt" "../solution.rkt"
         (only-in racket [remove-duplicates unique])
         (only-in "smtlib2.rkt" reset set-option check-sat get-model get-unsat-core query push pop)
         (only-in "../../base/core/term.rkt" term term? term-type)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv?)
         (only-in "../../base/core/real.rkt" @integer? @real?))

; Spacer solver is a fork of Z3 with improved fixepoint engine.
; TODO: remove amount of copy-paste from Z3

(provide (rename-out [make-spacer spacer]) spacer?)

(define-runtime-path spacer-path (build-path ".." ".." ".." "bin" "spacer"))
(define spacer-opts '("-in"))

(define (make-spacer)
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
    (if (empty? real-spacer-path) (spacer)
        (spacer (server real-spacer-path spacer-opts) '() '() '() '() (env) '()))))

(struct spacer (server asserts mins maxs rules env level)
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<spacer>"))]
  #:methods gen:solver
  [
   (define (solver-assert self bools)
     (set-spacer-asserts! self
      (append (spacer-asserts self)
              (for/list ([b bools] #:unless (equal? b #t))
                (unless (or (boolean? b) (and (term? b) (equal? @boolean? (term-type b))))
                  (error 'assert "expected a boolean value, given ~s" b))
                b))))

   (define (solver-minimize self nums)
     (set-spacer-mins! self (append (spacer-mins self) (numeric-terms nums 'solver-minimize))))

   (define (solver-maximize self nums)
     (set-spacer-maxs! self (append (spacer-maxs self) (numeric-terms nums 'solver-maximize))))

   (define (solver-clear self)
     (solver-clear-stacks! self)
     (solver-clear-env! self)
     (server-write (spacer-server self) (reset-default-options)))

   (define (solver-shutdown self)
     (solver-clear self)
     (server-shutdown (spacer-server self)))

   (define (solver-push self)
     (match-define (spacer server (app unique asserts) (app unique mins) (app unique maxs) _ env level) self)
     (server-write
      server
      (begin
        (encode env asserts mins maxs)
        (push)))
     (solver-clear-stacks! self)
     (set-spacer-level! self (cons (dict-count env) level)))

   (define (solver-pop self [k 1])
     (match-define (spacer server _ _ _ _ env level) self)
     (when (or (<= k 0) (> k (length level)))
       (error 'solver-pop "expected 1 < k <= ~a, given ~a" (length level) k))
     (server-write server (pop k))
     (solver-clear-stacks! self)
     (for ([lvl level][i k])
       (clear! env lvl))
     (set-spacer-level! self (drop level k)))

   (define (solver-check self)
     (match-define (spacer server (app unique asserts) (app unique mins) (app unique maxs) _ env _) self)
     (cond [(ormap false? asserts) (unsat)]
           [else (server-write
                  server
                  (begin (encode env asserts mins maxs)
                         (check-sat)
                         (get-model)))
                 (solver-clear-stacks! self)
                 (server-read server (decode env))]))

   (define (solver-debug self)
     (match-define (spacer server (app unique asserts) _ _ _ _ _) self)
     (cond [(ormap false? asserts) (unsat (list #f))]
           [else (solver-clear-env! self)
                 (server-write (spacer-server self) (reset-core-options))
                 (server-write
                  server
                  (begin (encode-for-proof (spacer-env self) asserts)
                         (check-sat)
                         (get-unsat-core)))
                 (server-read server (decode (spacer-env self)))]))]
  #:methods gen:horn-solver
  [
   (define (solver-add-rules self rules)
     (set-spacer-rules! self (append (spacer-rules self) rules)))

   (define (solver-query self q)
     (server-write
      (spacer-server self)
      (begin (encode-rules (spacer-env self) (spacer-rules self))
             (query (ref! (spacer-env self) q))
             (check-sat))) ; TODO: understand why it hangs without check-sat
     (solver-clear-stacks! self)
     (server-read (spacer-server self) (decode/generalized (spacer-env self))))]
  )

(define (reset-default-options)
  (reset)
  (set-option ':fixedpoint.engine 'spacer)
  (set-option ':fixedpoint.xform.slice 'false)
  (set-option ':fixedpoint.xform.inline_linear 'false)
  (set-option ':fixedpoint.xform.inline_eager 'false)
  (set-option ':fixedpoint.xform.tail_simplifier_pve 'false)
  (set-option ':fixedpoint.spacer.elim_aux 'true)
  (set-option ':fixedpoint.spacer.reach_dnf 'false)
  (set-option ':produce-unsat-cores 'false)
  (set-option ':auto-config 'true)
  (set-option ':smt.relevancy 2)
  (set-option ':smt.mbqi.max_iterations 10000000))

(define (reset-core-options)
  (reset)
  (set-option ':produce-unsat-cores 'true)
  (set-option ':auto-config 'false)
  (set-option ':smt.relevancy 0))

(define (numeric-terms ts caller)
  (for/list ([t ts] #:unless (or (real? t) (bv? t)))
    (match t
      [(term _ (or (== @integer?) (== @real?) (? bitvector?))) t]
      [_ (error caller "expected a numeric term, given ~s" t)])))

(define (solver-clear-stacks! self)
  (set-spacer-asserts! self '())
  (set-spacer-mins! self '())
  (set-spacer-maxs! self '()))

(define (solver-clear-env! self)
  (set-spacer-env! self (env))
  (set-spacer-level! self '()))
