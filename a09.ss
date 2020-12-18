;austin swatek
;a09

;1
 (define snlist-recur
    (lambda (base-val lst-proc sym-proc)
              (letrec
                ([helper
                   (lambda (snlst)
                     (cond[(null? snlst) base-val]
                       [(list? (car snlst)) (lst-proc (helper (car snlst)) (helper (cdr snlst)))]
                       [else (sym-proc (car snlst) (helper (cdr snlst)))]))])
                helper)))
 (define sn-list-sum
      (snlist-recur 0 + +))
 (define sn-list-map
    (lambda (proc lst)
      ((snlist-recur '() cons
         (lambda (x y)
           (cons (proc x) y)))
       lst)))
 (define sn-list-paren-count
    (snlist-recur 2 +
      (lambda (x y)
        y)))
 (define sn-list-reverse
    (snlist-recur '()
      (lambda (x y)
        (append y (list x)))
      (lambda (x y)
        (append y (list x)))))
 (define sn-list-occur
    (lambda (thing lst)
      ((snlist-recur 0 +
         (lambda (x y)
           (+
            (if (eq? thing x)
                1 0)
            y)))
       lst)))
 (define sn-list-depth
    (snlist-recur 1
      (lambda (x y)
        (if (>= x y)
            (+ x 1)
            y))
      (lambda (x y)
        y)))

;2
 (define bt-recur
    (lambda (base-val num-proc sym-proc)
      (letrec([helper (lambda (bt)
                        (cond[(null? bt) base-val]
                          [(number? bt) (num-proc bt)]
                          ;[(list? bt) (lst-proc (helper (cadr bt)) bt (helper (caddr bt)))]
                          ;[(symbol? bt) (sym-proc (helper (cadr bt)) bt (helper (caddr bt)))]
                          [else (sym-proc (helper (cadr bt)) bt (helper (caddr bt)))]
                          ))])
        helper)))
 (define bt-sum
    (bt-recur 0 + (lambda (x y z) (+ x z))))
 (define bt-inorder
    (bt-recur '() 
      (lambda (x) '()) 
      (lambda (x y z) (append x (list (car y)) z))
      ))