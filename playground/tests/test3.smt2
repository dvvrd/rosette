; Random-sum example (MuZ already can`t prove this one)
; Correct answer: unsat
; C-pseudocode:
;
; int random_sum(int n) {
;	if (n <= 0) return 0;
; 	return rand() + 1 + random_sum(n-1);
; }
; int n = rand();
; assert(random-sum(n) >= n);

(declare-rel random-sum (Int Int))
(declare-var n Int)
(declare-var m Int)
(declare-var p Int)

(rule (=> (<= n 0) (random-sum n 0)))
(rule (=> (and (>= p 1) (> n 0) (random-sum (- n 1) m)) (random-sum n (+ m p))))

(declare-rel fail ())
(rule (=> (and (random-sum n m) (< m n)) fail))

(query fail :print-certificate true)
