 (define qsort
    (lambda (pred lst)
      (if (null? lst)
          '()
          (append 
            (qsort pred
              (filter-in (lambda (x)
                           (pred x (car lst)))
                (cdr lst)))
            (list (car lst))
            (qsort pred
              (filter-out (lambda (x)
                            (pred x (car lst)))
                (cdr lst)))))))