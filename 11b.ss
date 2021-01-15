;11b
;interpreter team
(load "chez-init.ss") ; put this file in the same folder, or add a pathname

;TEAM ONE

(define literal?
 (lambda (x)
  (or
   (number? x)
   (string? x)
   ;(and (pair? x) (andmap literal? x))
   (null? x)
   ;(symbol? x)
   (boolean? x)
   (vector? x)
   (char? x))))

(define lambdaargs? (lambda (ls)
 (cond
  [(integer? ls) #f]
  [(symbol? ls) #t]
  [else
   (if (null? ls)
    #t
    (if (not (symbol? (car ls)))
     #f
     (lambdaargs? (cdr ls))))])))

(define-datatype expression expression?
  [let-exp
    (defined (list-of symbol?))
    (definitions loexp?)
    (body loexp?)]
  [let*-exp
    (defined (list-of symbol?))
    (definitions loexp?)
    (body loexp?)]
  [letrec-exp
    (defined (list-of symbol?))
    (definitions loexp?)
    (body loexp?)]
  [if-exp
    (bool expression?)
    (trueExp loexp?)]
  [if-else-exp
    (bool expression?)
    (trueExp loexp?)
    (falseExp loexp?)]
  [set!-exp
    (var symbol?)
    (setVal loexp?)]
  [var-exp
   (id symbol?)]
  [lit-exp
    (id literal?)]
  
  
  [lambda-exp
   (id (lambda (x) (or (symbol? x) (list-of symbol? x))) )
   (body loexp?)]
  [app-exp
   (rator expression?)
   (rand loexp?)])

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define loexp?
  (lambda (ls)
    (or (expression? ls) (list-of expression? ls))))

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(literal? datum) (lit-exp datum)]
     [(null? datum) (list)]
     [(not (list? datum))
      (eopl:error 'parse-exp "expression is not a proper list: ~s" datum)]
     [(list? datum)
      (cond
       [(eqv? (1st datum) 'lambda)
        (cond
         [(not (> (length datum) 2)) ;(lambda + args)
          (eopl:error 'parse-exp "incorrect lambda length: ~s" datum)]
         [(not (lambdaargs? (2nd datum)))
          (eopl:error 'parse-exp "lambda args must by symbols: ~s" datum)]
	        [else (lambda-exp  (2nd  datum)
		        (map parse-exp (cddr datum)))])]
       [(eqv? (car datum) 'let)
        (cond
         [(not (> (length datum) 2)) ;(let + args + bodies)
          (eopl:error 'parse-exp "incorrect let length: ~s" datum)]
         [(not (list? (2nd datum)))
          (eopl:error 'parse-exp "expression not a list: ~s" datum)]
         [(not(andmap list? (2nd datum)))
          (eopl:error 'parse-exp "expression not a proper list: ~s" datum)]
         [(not(andmap (lambda (x) (eqv? 2 (length x))) (2nd datum)))
          (eopl:error 'parse-exp "exp ust be a list of length 2: ~s" datum)]
         [(not(andmap symbol? (map car (2nd datum))))
          (eopl:error 'parse-exp "vars in exp must be symbols: ~s" datum)]
         [else
          (let-exp
           (map car (2nd datum))
           (map parse-exp (map cadr (2nd datum)))
           (map parse-exp (cddr datum)))])]
       [(eqv? (car datum) 'let*)
        (cond
         [(not (> (length datum) 2)) ;(let* + args + bodies)
          (eopl:error 'parse-exp "incorrect let* length: ~s" datum)]
         [(not (list? (2nd datum)))
          (eopl:error 'parse-exp "expression not a list: ~s" datum)]  
         [(not(andmap list? (2nd datum)))
          (eopl:error 'parse-exp "expression not a proper list: ~s" datum)]
         [(not(andmap (lambda (x) (eqv? 2 (length x))) (2nd datum)))
          (eopl:error 'parse-exp "exp ust be a list of length 2: ~s" datum)]
         [(not(andmap symbol? (map car (2nd datum))))
          (eopl:error 'parse-exp "vars in exp must be symbols: ~s" datum)]
         [else
          (let*-exp
           (map car (2nd datum))
           (map parse-exp (map cadr (2nd datum)))
           (map parse-exp (cddr datum)))])]
       [(eqv? (car datum) 'letrec)
        (cond
         [(not (eqv? (length datum) 3))
          (eopl:error 'parse-exp "incorrect letrec length: ~s" datum)]
         [(not (list? (2nd datum)))
          (eopl:error 'parse-exp "expression not a list: ~s" datum)]  
         [(not(andmap list? (2nd datum)))
          (eopl:error 'parse-exp "expression not a proper list: ~s" datum)]
         [(not(andmap (lambda (x) (eqv? 2 (length x))) (2nd datum)))
          (eopl:error 'parse-exp "exp ust be a list of length 2: ~s" datum)]
         [(not(andmap symbol? (map car (2nd datum))))
          (eopl:error 'parse-exp "vars in exp must be symbols: ~s" datum)]
         [else
          (letrec-exp
           (map car (2nd datum))
           (map parse-exp (map cadr (2nd datum)))
           (map parse-exp (cddr datum)))])]
       [(eqv? (1st datum) 'if)  ;if & if-else
        (cond
         [(= (length datum) 3) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
         [(= (length datum) 4) (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (cadddr datum)))]
         [else (eopl:error 'parse-exp "if-exp has no bodies: ~s" datum)])]
       [(eqv? (1st datum) 'set!) ;set!
        (if (= (length datum) 3)
         (set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
         (eopl:error 'parse-exp "set!-exp has the wrong number of arguments: ~s" datum))]
       [else
        (app-exp
         (parse-exp (1st datum))
		       (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (datum)
    (cases expression datum
      [var-exp (id) id]
      [lit-exp (id) id]
      [lambda-exp (id body) 
       (append (list 'lambda id) (map unparse-exp body))]
      [let-exp (defined definitions body)
       (append (list 'let (map (lambda (x y) (list x (unparse-exp y))) defined definitions)) (map unparse-exp body))]
      [let*-exp (defined definitions body)
       (append (list 'let* (map (lambda (x y) (list x (unparse-exp y))) defined definitions)) (map unparse-exp body))]
      [letrec-exp (defined definitions body)
       (append (list 'letrec (map (lambda (x y) (list x (unparse-exp y))) defined definitions)) (map unparse-exp body))]
      [app-exp (rator rand)
       (cons (unparse-exp rator) (map unparse-exp rand))]
      [if-exp (bool trueExp) ;if
       (list 'if (unparse-exp bool) (unparse-exp trueExp))]
      [if-else-exp (bool trueExp falseExp) ;if-else
       (list 'if (unparse-exp bool) (unparse-exp trueExp) (unparse-exp falseExp))]
      [set!-exp (var setVal) ;set!
       (list 'set! (unparse-exp var) (unparse-exp setVal))]
      [else '(fuck)])))
