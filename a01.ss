;austin swatek
;a01

;#1
 (define interval-contains?
    (lambda (lst n)
      (and
        (<= (car lst) n)
        (>= (cadr lst) n))))
;#2
(define interval-intersects?
  (lambda (ls1 ls2)
    (and (>= (cadr ls1) (car ls2)) (<= (car ls1) (cadr ls2)))))
;#3
 (define interval-union
    (lambda (i1 i2)
      (if (interval-intersects? i1 i2)
        (list (list (min (car i1) (cadr i1) (car i2) (cadr i2)) (max (car i1) (cadr i1) (car i2) (cadr i2))))
        (list i1 i2))))
;#4
 (define first
    (lambda (lst)
      (car lst)))
 (define second 
    (lambda (lst)
      (cadr lst)))
 (define third
    (lambda (lst)
      (caddr lst)))
;#5
 (define make-vec-from-points
    (lambda (v1 v2)
      (list (- (first v2) (first v1))
        (- (second v2) (second v1))
        (- (third v2) (third v1)))))
;#6
 (define dot-product
    (lambda (v1 v2)
      (+
      (+ (* (first v1) (first v2))
       (* (second v1) (second v2)))
       (* (third v1) (third v2)))))
;#7
 (define vector-magnitude
    (lambda (v)
      (sqrt (dot-product v v))))
;#8
  (define distance
    (lambda (v1 v2)
      (sqrt
        (+
         (+ 
          (expt (- (first v2) (first v1)) 2)
          (expt (- (second v2) (second v1)) 2)
          )
         (expt (- (third v2) (third v1)) 2)
         ))))