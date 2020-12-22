;austin swatek
;exam1

;C1
; (define straight-helper?
;    (lambda (lst)
;      (andmap (lambda (x y) (equal? (- y x) 1)) lst)))
 (define straight-helper?
    (lambda (lst)
      (cond[(null? lst) #t]
        [(null? (cdr lst)) #t]
        [(equal? (- (cadr lst) (car lst)) 1) (straight-helper? (cdr lst))]
        [else #f])))
 (define straight?
    (lambda (lst)
      (straight-helper? (list-sort < lst))))
;C2
 (define snlist-recur
    (lambda (base-val lst-proc sym-proc)
              (letrec
                ([helper
                   (lambda (snlst)
                     (cond[(null? snlst) base-val]
                       [(list? (car snlst)) (lst-proc (helper (car snlst)) (helper (cdr snlst)))]
                       [else (sym-proc (car snlst) (helper (cdr snlst)))]))])
                helper)))
 (define snlist-flatten
    (snlist-recur '()
      (lambda (x y)
        (append x y))
      (lambda (x y)
        (cons x y))))
 (define flatten
    (snlist-recur '()
      (lambda (x y)
        (append x y))
      (lambda (x y)
        (cons x y))))
;C3
 (define element
    (lambda (tree)
      (if (not (list? tree))
          tree
          (car tree))))
 (define right
    (lambda (tree)
      (if (null? tree)
          '()
          (caddr tree))))
 (define left
    (lambda (tree)
      (if (null? tree)
          '()
          (cadr tree))))
 (define leaf?
    (lambda (tree)
      (not (symbol? (element tree)))))
 (define empty-tree?
    (lambda (tree)
      (if (or (equal? tree '( () ())) (null? tree))
          #t
          #f)))
 (define max-int
    (lambda (first second third)
      (cond[(and (null? (car first)) (null? (car third)))
            (list (car second) (cadr second) (cadr second))]
        [(null? (car first))
            (append
                  (if (> (cadr second) (cadr third))
                   (list (car second) (cadr second))
                   (list (car third) (cadr third)))
                  (list (cadr second)))]
        [(null? (car third))
             (append
                  (if (> (cadr first) (cadr second))
                   (list (car first) (cadr first))
                   (list (car second) (cadr second)))
                  (list (cadr second)))]
        [(and (>= (cadr first) (cadr second)) (>= (cadr first) (cadr third))) (list (car first) (cadr first) (cadr second))]
        [(and (>= (cadr third) (cadr second)) (>= (cadr third) (cadr first))) (list (car third) (cadr third) (cadr second))]
        [else (list (car second) (cadr second) (cadr second))])))
 (define max-int-help
    (lambda (tree)
      (cond[(number? tree) (list '() tree tree)]
        [(list? tree)
         (let([l (max-int-help (left tree))]
              [r (max-int-help (right tree))])
         (max-int
           l
           (list (element tree) (+ (caddr l) (caddr r)))
           r))])))
 (define bt-max-interior
  (lambda (tree)
    (car (max-int-help tree))))