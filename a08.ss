;austin swatek
;a08

;1
 (define make-stack
    (lambda ()
      (let ([stk '()])
        (lambda (msg . args)
          (case msg
            [(empty?) (null? stk)]
            [(push) (set! stk (cons (car args) stk))]
            [(pop) (let ([top (car stk)])
                     (set! stk (cdr stk))
                     top)]
            [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))
 (define stk-next
    (lambda (stk)
      (if (stk 'empty?)
          #f
          (let ([top (stk 'pop)])
            (cond[(symbol? top) top]
              [(not (null? top))
               (stk 'push (cdr top))
               (stk 'push (car top))
               (stk-next stk)]
              [else (stk-next stk)])))))
 (define make-slist-leaf-iterator
    (lambda (lst)
      (let ([stk (make-stack)])
        (null? lst) #f
        (stk 'push (cdr lst))
        (stk 'push (car lst))
        (lambda (arg)
          (case arg
            [(next)
             (if (null? lst)
                 #f
                 (stk-next stk))])))))
;2
 (define subst-help
    (lambda (new old lst comp?)
      (cond[(null? lst) (list '() #f)]
        [(symbol? lst)
         (if (comp? old lst)
             (list new #t)
             (list lst #f))]
        [else
          (let* ([top-car (subst-help new old (car lst) comp?)]
                 [lst-car (car top-car)]
                 [bool-car (cadr top-car)])
            (if bool-car
                (list (cons lst-car (cdr lst)) #t)
                (let* ([top-cdr (subst-help new old (cdr lst) comp?)]
                       [lst-cdr (car top-cdr)]
                       [bool-cdr (cadr top-cdr)])
                  (list (cons lst-car lst-cdr) bool-cdr))))])))
 (define subst-leftmost
    (lambda (new old lst comp?)
      (car (subst-help new old lst comp?))))