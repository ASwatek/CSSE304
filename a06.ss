;austin swatek
;a06

;a
;1
 (define curry2
    (lambda (proc)
      (lambda (x)
        (lambda (y)
          (proc x y)))))
;2
 (define curried-compose
    (lambda (proc1)
      (lambda (proc2)
        (lambda (x)
          (proc1 (proc2 x))))))
;3
;given that its 2 or 3 procedures, surely a better way?
 (define compose
    (lambda lof
      (lambda (x)
        (if (null? (cddr lof))
            ((car lof) ((cadr lof) x))
            ((car lof) ((cadr lof) ((caddr lof) x)))))))
;4
 (define make-list-c
    (lambda (num)
      (lambda (val)
        (if (equal? num 0)
            '()
            (cons val ((make-list-c (- num 1)) val))))))
;5
 (define reverse-it
    (lambda (lst)
      (if (null? lst)
          '()
          (append (reverse-it (cdr lst)) (list (car lst))))))
;6
 (define map-by-position
    (lambda (fns args)
      (map apply fns (map list args))))
;7
 (define empty-BST
    (lambda ()
      '()))
 (define empty-BST?
    (lambda (bst)
      (equal? (empty-BST) bst)))
 (define BST-element
    (lambda (bst)
      (if (empty-BST? bst)
          '()
          (car bst))))
 (define BST-right
    (lambda (bst)
      (if (empty-BST? bst)
          '()
          (caddr bst))))
 (define BST-left
    (lambda (bst)
      (if (empty-BST? bst)
          '()
          (cadr bst))))
 (define BST-insert
    (lambda (num bst)
      (cond[(empty-BST? bst) (list num (empty-BST) (empty-BST))]
        [(< num (BST-element bst))
         (list (BST-element bst) (BST-insert num (BST-left bst)) (BST-right bst))]
        [(> num (BST-element bst))
         (list (BST-element bst) (BST-left bst) (BST-insert num (BST-right bst)))]
        [else bst])))
 (define BST-contains?
    (lambda (bst num)
      (cond[(empty-BST? bst) #f]
        [(equal? num (BST-element bst)) #t]
        [(< num (BST-element bst)) (BST-contains? (BST-left bst) num)]
        [(> num (BST-element bst)) (BST-contains? (BST-right bst) num)])))
 (define BST-inorder
	(lambda (bst)
		(if (empty-BST? bst)
			'()
			(append (BST-inorder (BST-left bst)) (list (BST-element bst)) (BST-inorder (BST-right bst))))))
 (define BST-insert-nodes
    (lambda (bst nums)
      (if (null? nums)
          bst
          (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))
 (define BST?
    (lambda (bst)
      (cond[(empty-BST? bst) #t]
        [(not (list? bst)) #f]
        [(not (integer? (BST-element bst))) #f]
        [(not (equal? (length bst) 3)) #f]
        [(and (empty-BST? (BST-left bst)) (empty-BST? (BST-right bst))) #t]
        [(not (or (BST? (BST-left bst)) (BST? (BST-right bst)))) #f]
        [(not (apply < (BST-inorder bst))) #f]
        [else #t])))
 (define BST-height
    (lambda (bst)
      (cond[(empty-BST? bst) -1]
        [else (max (+ (BST-height (BST-left bst)) 1) (+ (BST-height (BST-right bst)) 1))])))

;b
;8
 (define let->application
    (lambda (exp)
      (let([front (map car (cadr exp))]
           [back (map cadr (cadr exp))]
           [body (caddr exp)])
        (cons (list 'lambda front body) back))))
;9
 (define let*-helper
    (lambda (bindings body)
      (if (null? bindings)
          body
          (list 'let (list (car bindings)) (let*-helper (cdr bindings) body)))))
 (define let*->let
    (lambda (let*-exp)
      (let([bindings (cadr let*-exp)]
           [body (caddr let*-exp)])
        (let*-helper bindings body))))
;10
 (define partition
    (lambda (pred lst pivot lower greater)
      (if (null? lst)
          (list lower greater)
          (let ([element (car lst)])
            (if (pred pivot element)
                (partition pred (cdr lst) pivot lower (cons element greater))
                (partition pred (cdr lst) pivot (cons element lower) greater))))))
 (define qsort
    (lambda (pred lst)
      (cond[(null? lst) lst]
        [(null? (cdr lst)) lst]
        [else (let ([pivot (car lst)]
                    [sorted (partition pred (cdr lst) (car lst) '() '())])
                (append (qsort pred (car sorted)) (list pivot) (qsort pred (cadr sorted))))])))
;11
 (define sort-list-of-symbols
    (lambda (lst)
      (let([strings (map symbol->string lst)])
        (map string->symbol (sort string<? strings)))))