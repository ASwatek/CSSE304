
; test 1

(named-let->letrec
 '(let fact ([n 5] [acc 1])
    (if (zero? n)
	acc
	(fact (sub1 n) (* n acc)))))

; test1 answer
(letrec ([fact (lambda (n acc)
                 (if (zero? n) acc (fact (sub1 n) (* n acc))))])
  (fact 5 1))

; test2
(named-let->letrec
 '(let loop ([start 5])
    (display start)
    (set! start (sub1 start))
    (if (>= start 0)
	(loop start))))

; test2 answer
(letrec ([loop (lambda (start)
                 (display start)
                 (set! start (sub1 start))
                 (if (>= start 0) (loop start)))])
  (loop 5))

; test3
(named-let->letrec
 '(let sum-of-diffs ([low 4] [high 15] [sum 0])
    (if (> low high)
	sum
	(sum-of-diffs (add1 low) (sub1 high) (+ sum (- high low))))))

;test3 answer
(letrec ([sum-of-diffs (lambda (low high sum)
                         (if (> low high)
                             sum
                             (sum-of-diffs
                               (add1 low)
                               (sub1 high)
                               (+ sum (- high low)))))])
  (sum-of-diffs 4 15 0))

