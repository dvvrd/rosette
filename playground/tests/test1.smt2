; Simply proving that all fibonacci numbers are positive, easy
; Correct answer: unsat

(declare-rel fib (Int Int))
(declare-var n Int)
(declare-var m Int)
(declare-var p Int)
(declare-rel fail ())

(rule (=> (< n 2) (fib n 1)))
(rule (=> (and (>= n 2) (fib (- n 1) m) (fib (- n 2) p)) (fib n (+ m p))))

(rule (=> (and (fib n m) (<= m 0)) fail))
(query fail :print-certificate true)
