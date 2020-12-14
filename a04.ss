;austin swatek
;a04

;1
 (define matrix-ref
    (lambda (m row col)
      (list-ref (list-ref m row) col)))
;2
 (define all-nums?
    (lambda (lst)
      (cond[(null? lst) #t]
        [(not (number? (car lst))) #f]
        [else (all-nums? (cdr lst))])))
 (define matrix?
    (lambda (m)
      (cond[(null? m) #f]
        [(not (list? m)) #f]
        [(null? (car m)) #f]
        [(not (list? (car m))) #f]
        [(not (andmap all-nums? m)) #f]
        [(null? (cdr m)) #t]
        [(not (eq? (length (car m)) (length (cadr m)))) #f]
        [else (matrix? (cdr m))])))
;3
 (define transpose
    (lambda (original ret)
      (cond[(null? original) ret]
        [(null? (car original)) ret]
        [else (transpose (map cdr original) (append ret (list (map car original))))])))
 (define matrix-transpose
    (lambda (m)
      (transpose m '())))
;4
 (define filter-in
    (lambda (pred? lst)
      (cond[(null? lst) '()]
        [(pred? (car lst)) (cons (car lst) (filter-in pred? (cdr lst)))]
        [else (filter-in pred? (cdr lst))])))
;5
 (define reverso
    (lambda (lst)
      (list (cadr lst) (car lst))))
 (define invert
    (lambda (lst)
      (map reverso lst)))
;6
 (define fact
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (- n 1))))))
 (define choose
    (lambda (n k)
      (/ (fact n) (* (fact k) (fact (- n k))))))
 (define make-row
    (lambda (n curr)
      (if (negative? curr)
          '()
          (cons (choose n curr) (make-row n (- curr 1))))))
 (define pascal-triangle
    (lambda (n)
      (cond[(negative? n) '()]
        [(zero? n) '((1))]
        [else (cons (make-row n n) (pascal-triangle (- n 1)))])))