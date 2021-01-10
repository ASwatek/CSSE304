;austin swatek
;a10

;1
 (define helper
  (lambda (x y)
    (cond[(null? x) y]
      [(symbol? x)
       (if (memq x y)
           y
           (cons x y))]
      [else (helper (car x) (helper (cdr x) y))])))
 (define free-vars
  (case-lambda
    [(func) (free-vars func '() '())]
    [(func free bound)
     (cond [(null? func) free]
       [(symbol? func)
        (if (memq func bound)
            free
            (helper func free))]
       [(eq? (car func) 'lambda) (free-vars (cddr func) free (helper (cadr func) bound))]
       [(null? (cdr func)) (free-vars (car func) free bound)]
       [else (helper (free-vars (car func) free bound) (free-vars (cadr func) free bound))])]))
 (define bound-vars
  (case-lambda
    [(func) (bound-vars func '())]
    [(func bound)
     (cond[(null? func) '()]
       [(symbol? func)
        (if (memq func bound)
            (list func)
            '())]
       [(eq? (car func) 'lambda) (bound-vars (cddr func) (append (cadr func) bound))]
       [(null? (cdr func)) (bound-vars (car func) bound)]
       [else (append (bound-vars (car func) bound) (bound-vars (cadr func) bound))])]))
;2
 (define let->application
    (lambda (exp)
      (let([front (map car (cadr exp))]
           [back (map cadr (cadr exp))]
           [body (caddr exp)])
        (cons (list 'lambda front body) back))))
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
 (define occurs-free?
    (lambda (var func)
      (let([free? (lambda (curr) (occurs-free? var curr))])
        (cond[(symbol? func) (eq? var func)]
          [(eq? (car func) 'lambda)
           (and (not (member var (cadr func))) (free? (cddr func)))]
          [(eq? (car func) 'if)
           (ormap free? (cdr func))]
          [(eq? (car func) 'let)
           (free? (let->application func))]
          [(eq? (car func) 'let*)
           (free? (let->application (let*->let func)))]
          [(eq? (car func) 'set!)
           (free? (cddr func))]
          [else (ormap free? func)]))))
 (define occurs-bound?
    (lambda (var func)
      (cond [(symbol? func) #f]
        [(null? func) #f]
        [(eq? (car func) 'lambda)
         (or (occurs-bound? var (caddr func))
             (and (memv var (cadr func)) (occurs-free? var (caddr func))))]
        [(eq? (car func) 'let)
         (occurs-bound? var (let->application func))]
        [(eq? (car func) 'let*)
         (occurs-bound? var (let*->let func))]
        [(eq? (car func) 'if)
         (or (occurs-bound? var (cadr func)) (occurs-bound? var (caddr func))
             (and (not (null? (cdddr func))) (occurs-bound? var (cadddr func))))]
        [(eq? (car func) 'set!)
         (or (occurs-bound? var (caddr func))
             (and (eq? var (cadr func))
                  (occurs-free? var (caddr func))))]
        [else (or (occurs-bound? var (car func)) (occurs-bound? var (cdr func)))])))
;3
 (define lex-help
    (lambda (var lst)
      (if (null? lst)
          'free
          (if (eqv? var (caar lst))
              (cdar lst)
              (lex-help var (cdr lst))))))
 (define depth-help
   (lambda (lst)
     (let make ((left lst) (built '()))
       (if (null? left)
           built
           (make (cdr left) (append built (list (list (caar left) (+ (cadar left) 1) (caddar left)))))))))
 (define lex-ins
   (lambda (bound new)
     (let ((newer
             (let build ((left new) (p 0) (built '()))
               (if (null? left)
                   built
                   (build (cdr left) (+ p 1) (append built (list (list (car left) 0 p)))))))
           (older
             (let build ((left new) (built bound))
               (if (null? left)
                   built
                   (build (cdr left) (remv '() (map (lambda (x) (if (eq? (car x) (car left)) '() x)) built)))))))
       (append newer older))))
 (define add-help
   (lambda (func bound)
     (cond[(symbol? func) (let ((info (lex-help func bound)))
                            (if (eq? info 'free)
                                (list ': 'free func)
                                (list ': (car info) (cadr info))))]
       [(eq? (car func) 'lambda)
        (list 'lambda (cadr func) (add-help (caddr func) (lex-ins (depth-help bound) (cadr func))))]
       [(eq? (car func) 'if) (cons 'if (map (lambda (x) (add-help x bound)) (cdr func)))]
       [(eq? (car func) 'set!) (list 'set! (list (map (lambda (x) (add-help x bound)) (cdr func)) (map (lambda (x) (add-help x bound)) (cddr func))))]
       [else (map (lambda (x) (add-help x bound)) func)])))
 (define lexical-address
   (lambda (func)
     (add-help func '())))
;4
 (define lex-find
   (lambda (var lst)
     (if (and (eq? (car var) (cadar lst)) (eq? (cadr var) (caddar lst)))
         (caar lst)
         (lex-find var (cdr lst)))))
 (define un-help
   (lambda (func bound)
     (cond[(eq? (car func) ':)
           (if (eq? (cadr func) 'free)
               (caddr func)
               (lex-find (cdr func) bound))]
       [(eq? (car func) 'lambda)
        (list 'lambda (cadr func) (un-help (caddr func) (lex-ins (depth-help bound) (cadr func))))]
       [(eq? (car func) 'set!)
        	(list 'set! (cddr func) (un-help (cdddr func) (lex-ins (depth-help bound) (cddr func))))]
       [(eq? (car func) 'if) (cons 'if (map (lambda (x) (un-help x bound)) (cdr func)))]
       [else (map (lambda (x) (un-help x bound)) func)])))
 (define un-lexical-address
   (lambda (func)
     (un-help func '())))
