;austin swatek
;a11

 (load "C:/..desktopStuff/Work/Senior-Winter/CSSE304W/chez-init.ss")
 (define-datatype bintree bintree?
   (leaf-node
     (datum number?))
   (interior-node
     (key symbol?)
     (left-tree bintree?)
     (right-tree bintree?)))

;1
 (define-syntax my-let
   (syntax-rules ()
     [(_ ([var num] ...) e1 e2 ...)
      ((lambda (var ...) e1 e2 ...)
       num ...)]
     [(_ name ([var num] ...) e1 e2 ...)
      (letrec ([name (lambda (var ...) e1 e2 ...)])
        (name num ...))]))
 (define-syntax my-or
   (syntax-rules ()
     [(_) #f]
     [(_ e1) e1]
     [(_ e1 e2 ...)
      (let ([func e1])
        (if func
            func
            (my-or e2 ...)))]))
 (define-syntax +=
   (syntax-rules ()
     [(_ var num) (begin (set! var (+ var num)) var)]))
 (define-syntax return-first
   (syntax-rules ()
     [(_ e1) e1]
     [(_ e1 e2 ...)
      (let ([func e1])
        (begin e2 ... func))]))

;2
 (define bintree-to-list
   (lambda (tree)
     (cases bintree tree
       (leaf-node (num) (list 'leaf-node num))
       (interior-node (key left right) (list 'interior-node key (bintree-to-list left) (bintree-to-list right))))))
;3
 (define find-max
   (lambda (nums syms)
     (let([maxVal (max (car nums) (cadr nums) (caddr nums))])
       (cond[(and (null? (car syms)) (null? (cadr syms))) (list (caddr nums) (caddr syms))]
         [(list? (car syms))
          (if (> (cadr nums) (caddr nums))
              (list (cadr nums) (cadr syms))
              (list (caddr nums) (caddr syms)))]
         [(list? (cadr syms))
          (if (> (car nums) (caddr nums))
              (list (car nums) (car syms))
              (list (caddr nums) (caddr syms)))]
         [(eq? maxVal (car nums)) (list (car nums) (car syms))]
         [(eq? maxVal (cadr nums)) (list (cadr nums) (cadr syms))]
         [else (list (caddr nums) (caddr syms))]))))
 (define max-help
   (lambda (tree)
     (cases bintree tree
       (leaf-node (num) (list num num '()))
       (interior-node (key left right)
         (let([leftTree (max-help left)]
              [rightTree (max-help right)])
           (let([center (+ (car leftTree) (car rightTree))])
             (append (list center) (find-max (list (cadr leftTree) (cadr rightTree) center) (list (caddr leftTree) (caddr rightTree) key)))))))))
 (define max-interior
   (lambda (tree)
     (caddr (max-help tree))))