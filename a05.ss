;austin swatek
;a05

;1
 ;(list-sort (lambda (x y) (< (car x) (car y))) '())
 (define sort-it
    (lambda (lst1 lst2)
      (cond [(not (equal? (car lst1) (car lst2))) (< (car lst1) (car lst2))]
        [(not (equal? (cadr lst1) (cadr lst2))) (< (cadr lst1) (cadr lst2))])))
 (define overlaps?
    (lambda (lst1 lst2)
      (if (>= (cadr lst1) (car lst2))
          #t
          #f)))
 (define merge
    (lambda (lst1 lst2)
      (list
        (min (car lst1) (cadr lst1) (car lst2) (cadr lst2))
        (max (car lst1) (cadr lst1) (car lst2) (cadr lst2)))))
 (define minimize-interval-list
    (lambda (lst)
      (let [(sorted (list-sort sort-it lst))]
        (cond[(or (null? lst) (null? (car lst)) (null? (cdr lst)) (null? (cadr lst))) lst]
          [(overlaps? (car sorted) (cadr sorted))
           (minimize-interval-list (cons (merge (car sorted) (cadr sorted)) (cddr sorted)))]
          [else (append (list (car sorted)) (minimize-interval-list (cdr sorted)))]))))
;2
 (define exists?
    (lambda (pred lst)
      (ormap pred lst)))
;3
 (define single-list
    (lambda (sym lst)
      (if (null? lst)
          '()
          (cons (list sym (car lst)) (single-list sym (cdr lst))))))
 (define product
    (lambda (set1 set2)
      (if (or (null? set1) (null? set2))
          '()
          (append (single-list (car set1) set2) (product (cdr set1) set2)))))
;4
 (define replace
    (lambda (old new lst)
      (cond[(null? lst) '()]
        [(equal? (car lst) old) (cons new (replace old new (cdr lst)))]
        [else (cons (car lst) (replace old new (cdr lst)))])))
;5
 (define remove-first
    (lambda (sym lst)
      (cond[(null? lst) '()]
        [(equal? sym (car lst)) (cdr lst)]
        [else (cons (car lst) (remove-first sym (cdr lst)))])))
 (define remove-last
    (lambda (sym lst)
      (reverse (remove-first sym (reverse lst)))))