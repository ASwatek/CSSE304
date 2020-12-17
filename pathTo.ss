 (define path-to
    (lambda (lst sym)
      (let pathto ([lst lst] [path-so-far '()])
        (cond[(null? lst) #f]
          [(eq? (car lst) sym)
           (reverse (cons 'car path-so-far))]
          [(symbol? (car lst))
           (pathto (cdr lst) (cons 'cdr path-so-far))]
          [else
            (or (pathto (car lst) (cons 'car path-so-far))
                (pathto (cdr lst) (cons 'cdr path-so-far)))]))))