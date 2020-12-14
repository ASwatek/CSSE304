;austin swatek
;a03

;1
 (define contains?
  (lambda (lst item)
   (cond[(null? lst) #f]
        [(equal? (car lst) item) #t]
        [(null? (cdr lst)) #f]
        [else (contains? (cdr lst) item)])))
 (define intersection
    (lambda (s1 s2)
      (cond [(or (null? s1) (null? s2)) '()]
        [(contains? s2 (car s1)) (cons (car s1) (intersection (cdr s1) s2))]
        [else (intersection (cdr s1) s2)])))

;2
 (define subset?
    (lambda (s1 s2)
      (cond [(null? s1) #t]
        [(equal? (intersection s1 s2) s1) #t]
        [else #f])))
;3
 (define set?
  (lambda (ls)
    (cond [(null? ls) #t]
          [(not (list? ls)) #f]
          [(not (list? (car ls))) #f]
          [(contains? (cdr ls) (car ls)) #f]
          [else (set? (cdr ls))])))
 (define size
    (lambda (lst)
      (if (null? lst)
          0
          (+ 1 (size (cdr lst))))))
 (define relation-helper?
    (lambda (original lst truf)
      (cond [(equal? truf #f) #f]
        [(null? lst) #t]
        [(not (set? lst)) #f]
        [(or (contains? original (car lst)) (contains? (cdr lst) (car lst))) #f]
        [(not (equal? (size (car lst)) 2)) #f]
        [else (relation-helper? (append original (car lst)) (cdr lst) truf)])))
 (define relation?
    (lambda (lst)
      (relation-helper? '() lst #t)))

;4
  (define domain-helper
    (lambda (domain lst)
      (cond[(null? lst) '()]
        [(contains? domain (caar lst)) (domain-helper domain (cdr lst))]
        [else (cons (caar lst) (domain-helper (cons (caar lst) domain) (cdr lst)))])))
 (define domain
    (lambda (lst)
      (domain-helper '() lst)))
;5
 (define reflexive-helper?
    (lambda (lst vals truf)
      (cond [(equal? #f truf) #f]
        [(null? lst) #t]
        [(null? vals) #t]
        [(contains? lst (list (car vals) (car vals))) (reflexive-helper? lst (cdr vals) truf)]
        [else #f]
        )))
 (define reflexive?
 (lambda (lst)
  (reflexive-helper? lst (append (domain lst) (domain (map reverse lst))) #t)))
;6
 (define si-set?
   (lambda (lst)
     (cond [(null? lst) #t]
       [(not (list? lst)) #f]
       [(not (symbol? (car lst))) #f]
       [(not (integer? (cadr lst))) #f]
       [(< (cadr lst) 1) #f]
       [else #t])))
 (define all-true?
    (lambda (lst)
      (cond [(null? lst) #t]
        [(equal? (car lst) #f) #f]
        [else (all-true? (cdr lst))])))
 (define multi-set?
    (lambda (lst)
      (all-true? (map si-set? lst))))
 (define no-duplicates?
    (lambda (checked unchecked)
      (cond[(null? unchecked) #t]
        [(contains? checked (car unchecked)) #f]
        [else (no-duplicates? (cons (car unchecked) checked) (cdr unchecked))])))
 (define multi-set?
    (lambda (lst)
      (if (not (list? lst))
          #f
          (and (all-true? (map si-set? lst)) (no-duplicates? '() (map car lst))))))

 ;(define multi-set?
  ;  (lambda (lst)
   ;   (cond[(null? lst) #t]
    ;    [(not (list? (car lst))) #f]
     ;   [(not (symbol? (caar lst))) #f]
      ;  [(not (integer? (cadar lst))) #f]
       ; [(not (> (cadar lst) 0)) #f]
        ;[else (multi-set? (cdr lst))])))






;7
 (define ms-size
    (lambda (ms)
      (cond [(null? ms) 0]
        [else (apply + (map cadr ms))])))
;8
 (define last
    (lambda (lst)
      (if (null? (cdr lst))
          (car lst)
          (last (cdr lst)))))
;9
 (define all-but-last
    (lambda (lst)
      (if (or (null? lst) (null? (cdr lst)))
        '()
         (cons (car lst) (all-but-last (cdr lst))))))