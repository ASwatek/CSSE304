;austin swatek
;a07

;part a
;1
 (define base-vector
    (lambda (v ret vlen index)
      (cond[(>= index vlen) ret]
        [else (vector-set! ret index (vector-ref v index))
          (base-vector v ret vlen (+ index 1))])))
 (define vector-list-help
    (lambda (ret lst veclen index)
      (cond[(null? lst) ret]
      [else (vector-set! ret (+ veclen index) (car lst))
        (vector-list-help ret (cdr lst) veclen (+ 1 index))])))
 (define vector-append-list
    (lambda (v lst)
      (let([vlen (vector-length v)]
           [lstlen (length lst)])
        (vector-list-help 
          (base-vector v (make-vector (+ lstlen vlen)) vlen 0)
          lst
          vlen
          0))))
;2
 (define group-by-two
    (lambda (lst)
      (cond
        [(null? lst) '()]
        [(null? (cdr lst)) (list (list (car lst)))]
        [else (cons (list (car lst) (cadr lst)) (group-by-two (cddr lst)))])))
;3
 (define n-words
    (lambda (lst n)
      (if (or (null? lst) (zero? n))
          '()
          (cons (car lst) (n-words (cdr lst) (- n 1))))))
 (define remaining
    (lambda (lst n)
      (cond[(null? lst) '()]
        [(zero? n) lst]
        [else (remaining (cdr lst) (- n 1))])))
 (define group-by-n
    (lambda (lst n)
      (cond[(null? lst) '()]
        [(null? (cdr lst)) (list lst)]
        [else (cons (n-words lst n) (group-by-n (remaining lst n) n))])))
;4
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
 (define bt-leaf-sum
    (lambda (tree)
      (cond[(empty-tree? tree) 0]
        [(not (leaf? tree)) (+ (bt-leaf-sum (left tree)) (bt-leaf-sum (right tree)))]
        [else (element tree)])))
 (define bt-inorder-list
    (lambda (tree)
      (cond[(empty-tree? tree) '()]
        [(leaf? tree) '()]
        [(leaf? (left tree)) (cons (element tree) (bt-inorder-list (right tree)))]
        [else (append (bt-inorder-list (left tree)) (list (element tree)) (bt-inorder-list (right tree)))])))
 (define bt-max
    (lambda (tree)
      (cond[(not (list? tree)) tree]
        [(empty-tree? tree) tree]
        [else (max (bt-max (left tree)) (bt-max (right tree)))])))
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
;part b
;5
 (define slist-map
    (lambda (proc slist)
      (cond[(null? slist) '()]
        [(symbol? slist) (proc slist)]
        [(symbol? (car slist)) (cons (proc (car slist)) (slist-map proc (cdr slist)))]
        [(list? slist) (cons (slist-map proc (car slist)) (slist-map proc (cdr slist)))])))
 (define slist-reverse
    (lambda (slist)
      (cond[(null? slist) '()]
        [(symbol? (car slist)) (append (slist-reverse (cdr slist)) (list (car slist)))]
        [else (append (slist-reverse (cdr slist)) (list (slist-reverse (car slist))))])))
 (define slist-paren-count
    (lambda (slist)
      (cond[(null? slist) 2]
        [(symbol? (car slist)) (slist-paren-count (cdr slist))]
        [(list? (car slist)) (+ (slist-paren-count (car slist)) (slist-paren-count (cdr slist)))])))
 (define slist-depth
    (lambda (slist)
      (cond[(null? slist) 1]
        [(symbol? (car slist)) (slist-depth (cdr slist))]
        [(list? (car slist)) (max (+ 1 (slist-depth (car slist))) (slist-depth (cdr slist)))])))
 (define only-symbols
    (lambda (lst)
      (cond[(null? lst) '()]
        [(symbol? (car lst)) (cons (car lst) (only-symbols (cdr lst)))]
        [else (only-symbols (cdr lst))])))
 (define slist-symbols-at-depth
    (lambda (slist n)
      (cond[(null? slist) '()]
        [(zero? n) '()]
        [(= n 1) (only-symbols slist)]
        [(list? (car slist)) (append (slist-symbols-at-depth (car slist) (- n 1)) (slist-symbols-at-depth (cdr slist) n))]
        [else (slist-symbols-at-depth (cdr slist) n)])))

;6
 (define pathing
    (lambda (slist thing)
      (cond[(null? slist) #f]
        [(null? (car slist)) (cons 'cdr (pathing (cdr slist) thing))]
        [(symbol? (car slist))
         (if (eq? (car slist) thing)
             (list 'car)
             (cons 'cdr (pathing (cdr slist) thing)))]
        [else (cons 'car (pathing (car slist) thing))])))
 (define path-to
    (lambda (slist thing)
      (let ([path (pathing slist thing)])
        (if (list? path)
            path
            #f))))
;7
 (define compose
    (case-lambda
      [() (lambda (x) x)]
      [(first . rest)
       (let ([composed-rest (apply compose rest)])
         (lambda (x) (first (composed-rest x))))]))
 (define proselytize
    (lambda (lst)
      (cond[(null? lst) '()]
        [(equal? #\a (list-ref lst 0)) (cons 'car (proselytize (cdr lst)))]
        [else (cons 'cdr (proselytize (cdr lst)))])))
 (define make-c...r
    (lambda (ada)
      (lambda (lst)
        (cond[(equal? ada "") lst]
          [else
            (let ([answer (proselytize (string->list ada))])
              ((apply compose (map eval answer)) lst))]))))

;couldnt beat you :/
; (define make-c...r
;    (lambda (ada)
;      (lambda (lst)
;        (let ([thing (string->list ada)])
;        (letrec ([answer lst]
;                 (proselytize
;                   (lambda (str)
;                     (cond[(null? str) answer]
;                       [(equal? #\a (car str))
;                        (set! answer (car answer))
;                        (proselytize (cdr thing))]
;                       [else
;                         (set! answer (cdr answer))
;                         (proselytize (cdr str))]))))
;          (proselytize thing))))))