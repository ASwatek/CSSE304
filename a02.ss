;austin swatek
;a02

;1
 (define fact
    (lambda (n)
      (if (eqv? n 0)
          1
          (* n (fact (- n 1))))))
 (define choose
    (lambda (n k)
      (/ (fact n) (* (fact k) (fact (- n k))))))

;2
 (define sum-of-squares
    (lambda (lst)
      (if (null? lst)
          0
          (+ (* (car lst) (car lst)) (sum-of-squares (cdr lst))))))
;3
 (define range
    (lambda (m n)
      (cond [(>= m n) '()] ;return space
        [(cons m (range (+ m 1) n))])))
;4
 (define contains?
    (lambda (lst item)
      (cond[(null? lst) #f]
        [(equal? (car lst) item) #t]
        [(null? (cdr lst)) #f]
      [else (contains? (cdr lst) item)])))
 (define set?
    (lambda (lst)
      (cond[(null? lst) #t]
        [(contains? (cdr lst) (car lst)) #f]
      [else (set? (cdr lst))])))
;5
 (define union
    (lambda (lst1 lst2)
      (cond[(null? lst2) lst1]
        [(null? (car lst2)) lst1]
        [(contains? lst1 (car lst2)) (union lst1 (cdr lst2))]
        [else (union (append lst1 (list (car lst2))) (cdr lst2))])))
;6
 (define first
    (lambda (lst)
      (car lst)))
 (define second 
    (lambda (lst)
      (cadr lst)))
 (define third
    (lambda (lst)
      (caddr lst)))
 (define cross-product
    (lambda (v1 v2)
      (list 
        (- (* (second v1) (third v2)) (* (third v1) (second v2)))
        (- (* (third v1) (first v2)) (* (first v1) (third v2)))
        (- (* (first v1) (second v2)) (* (second v1) (first v2))))))
;7 
 (define parallel?
    (lambda (v1 v2)
      (if (equal? '(0 0 0) (cross-product v1 v2))
          #t
          #f)))
;8
 (define make-vec-from-points
    (lambda (v1 v2)
      (list (- (first v2) (first v1))
        (- (second v2) (second v1))
        (- (third v2) (third v1)))))
 (define collinear?
    (lambda (p1 p2 p3)
      (if (equal? '(0 0 0)
            (cross-product 
              (make-vec-from-points p1 p2)
              (make-vec-from-points p2 p3)))
          #t
          #f)))
 (define zero-check?
    (lambda (v)
      (and (or (equal? (first v) 0) (equal? (first v) 0.0))
           (or (equal? (second v) 0) (equal? (second v) 0.0))
           (or (equal? (third v) 0) (equal? (third v) 0.0)))))
 (define collinear?
    (lambda (p1 p2 p3)
      (if (zero-check? 
            (cross-product 
              (make-vec-from-points p1 p2)
              (make-vec-from-points p2 p3)))
          #t
          #f)))