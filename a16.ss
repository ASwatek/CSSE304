;: Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

;(load "chez-init.ss") 


(define literal?
 (lambda (x)
  (or
   (number? x)
   (string? x)
   (null? x)
   (symbol? x)
   (boolean? x)
   (string? x)
   (list? x)
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



(define beginret
 (lambda (args)
  (if (null? (cdr args))
   (car args)
   (beginret (cdr args)))))

(define de2
 (lambda (args)
  (if (eqv? (car args) 'begin-exp)
   (beginret (cadr args))
   args)))


;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression.  You'll probably want to replace this 
; code with your expression datatype from A11b

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
(proc-names (list-of symbol?))
(idss (list-of (list-of symbol?)))
(bodiess (list-of (list-of expression?)))
(letrec-bodies (list-of expression?))]
 [namedlet-exp
  (name symbol?)
  (defined (list-of symbol?))
  (definitions loexp?)
  (bodies loexp?)]
 [if-exp
  (if-exp expression?)
  (bodies loexp?)]
 [set!-exp
  (var symbol?)
  (setVal loexp?)]
 [var-exp
  (id symbol?)]
 [lit-exp
  (id literal?)]
 [begin-exp 
  (body loexp?)]
 [cond-exp
  (cases loexp?)
  (results loexp?)]
 [while-exp
  (bool expression?)
  (bodies loexp?)]
 [lambda-exp
  (id (lambda (x) (or (symbol? x) (list-of symbol? x))) )
  (body loexp?)]
 [app-exp
  (rator expression?)
  (rand loexp?)]
  
  )
  
(define nf?
 (lambda (args)
  (or (symbol? args) (and (not (list? args)) (pair? args)))))
;; environment type definitions

(define scheme-value?
 (lambda (x)
  #t))
  
(define-datatype environment environment?
 [empty-env-record]
 [extended-env-record
  (syms (lambda (x) (or (nf? x) (list-of symbol? x))))
  (vals (list-of scheme-value?))
  (env environment?)]
  [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodiess (list-of (list-of expression?)))
    (old-env environment?)]
  
  
  
  
  )


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
 [prim-proc
  (name symbol?)]
 [closure
  (ids (lambda (x) (or (list-of symbol?) (nf? x))))
  (bodies (list-of expression?))
  (env environment?)])

  
;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define loexp?
 (lambda (ls)
  (or (expression? ls) (list-of expression? ls))))

; Again, you'll probably want to use your code form A11b

(define parse-exp         
 (lambda (datum)
  (cond
   [(symbol? datum) (var-exp datum)]
   [(number? datum) (lit-exp datum)]
   [(null? datum) (list)]
   [(not (list? datum)) (lit-exp datum)]
   [(eqv? (car datum) 'quote) (lit-exp (cadr datum))]
    [(list? datum)
     (cond
      [(eqv? (1st datum) 'lambda)
       (cond
        [(not (> (length datum) 2)) ;(lambda + args)
         (eopl:error 'parse-exp "incorrect lambda length: ~s" datum)]
        [(not (lambdaargs? (2nd datum)))
         (eopl:error 'parse-exp "lambda args must by symbols: ~s" datum)]
	       [else (lambda-exp  (2nd  datum) (map parse-exp (cddr datum)))])]
      [(eqv? (car datum) 'let)
       (cond
        [(not (> (length datum) 2)) ;(let + args + bodies)
         (eopl:error 'parse-exp "incorrect let length: ~s" datum)]
        [(symbol? (2nd datum)) ;(1st = let, 2nd = name
         (namedlet-exp (2nd datum) (map 1st (3rd datum)) (map parse-exp (map 2nd (3rd datum))) (map parse-exp (cdddr datum)))]
         ; (letrec-exp
          ; (2nd datum)
          ; (map cadr (map cadr (2nd datum)))
           ; (map (lambda (x) (map parse-exp x))
                  ; (map cddr (map cadr (2nd datum))))
          ; (map parse-exp (cddr datum)))]
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
      [(eqv? (car datum) 'begin)
       (begin-exp (map parse-exp (cdr datum)))]
      [(eqv? (car datum) 'cond)
       (cond-exp (map parse-exp (map car (cdr datum))) (map parse-exp (map cadr (cdr datum))))]
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
          (map cadr (map cadr (2nd datum)))
           (map (lambda (x) (map parse-exp x))
                  (map cddr (map cadr (2nd datum))))
          (map parse-exp (cddr datum)))])]
      [(eqv? (1st datum) 'if)  ;if & if-else
       (cond
        [(or
          (null? (cdr datum)) ; There must be conditions
          (null? (cddr datum)) ; There must be at least one body
          (and (not (null? (cdddr datum))) (not (null? (cddddr datum))))) ; There can be no more than 2 bodies
           (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
        [else (if-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]
      [(eqv? (1st datum) 'set!) ;set!
       (if (= (length datum) 3)
        (set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
        (eopl:error 'parse-exp "set!-exp has the wrong number of arguments: ~s" datum))]
      [(eqv? (1st datum) 'while) ;while
       (if (>= (length datum) 3)
        (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))
        (eopl:error 'while-exp "nice fuck up idiot: ~s" datum))]
      [else
       (app-exp
        (parse-exp (1st datum))
		      (map parse-exp (cdr datum)))])]
   [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+

; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3

(define extend-env-recursively
(lambda (proc-names idss bodiess old-env)
(recursively-extended-env-record
proc-names idss bodiess old-env)))

(define empty-env
 (lambda ()
  (empty-env-record)))

(define extend-env
 (lambda (syms vals env)
  (extended-env-record syms vals env)))

(define list-find-position
 (lambda (sym los)
  (let loop ([los los] [pos 0])
   (cond [(null? los) #f]
	   [(eq? sym (car los)) pos]
	   [else (loop (cdr los) (add1 pos))]))))
(define ptolist
 (lambda (p)
  (if (null? p)
   (list)
   (if (pair? p)
    (append (ptolist (car p)) (ptolist (cdr p)))
    (list p)))))

(define multivar
 (lambda (vals len)
  (if (eq? len 1)
   (list vals)
   (cons (car vals) (multivar (cdr vals) (- len 1))))))
	;defabetterwayformultivar    
(define apply-env
 (lambda (env sym) 
  (cases environment env 
   [empty-env-record () (apply-global-env sym)]
    
    [recursively-extended-env-record
      (procnames idss bodiess old-env)
      (let ([pos (list-find-position sym procnames)])
        (if (number? pos)
            (closure (list-ref idss pos)
              (list-ref bodiess pos)
              env)
            (apply-env old-env sym)))]
    
    
   [extended-env-record (syms vals env)
    (cond
     [(and (pair? syms) (not (list? syms)))
      (let* [(lsyms (ptolist syms))
        (lvals (multivar vals (length lsyms)))]
       (begin (set! syms lsyms) (set! vals lvals)
        (let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	         (list-ref vals pos)
	         (apply-env env sym)))))]
     [(symbol? syms)
      (let [(lsyms (list syms)) (lvals (list vals))]
       (begin (set! syms lsyms) (set! vals lvals)
        (let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	         (list-ref vals pos)
	         (apply-env env sym)))))]
     [else
	     (let ((pos (list-find-position sym syms)))
      	(if (number? pos)
	       (list-ref vals pos)
	       (apply-env env sym)))])])))

(define apply-global-env
 (lambda (sym)
  (cases environment init-env
   [extended-env-record (syms vals env)
    (let ([pos (list-find-position sym syms)])
     (if (number? pos)
      (list-ref vals pos)
      (eopl:error 'global-env "symbol ~s is not bound in global env" sym)))]
    [recursively-extended-env-record (proc-names idss bodiess old-env)
      (let ([pos (list-find-position sym proc-names)])
     (if (number? pos)
         (closure (list-ref idss pos)
      (list-ref bodiess pos)
           env)
         (apply-env old-env sym)))]
   [empty-env-record () (eopl:error 'global-env "THIS SHOULD NOT OCCUR")])))


;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+

(define condverter
 (lambda (cases results)
  (if (null? cases) (void)
   (if (eq? (cadr (car cases)) 'else)
    (car results)
    (if (null? (cdr cases))
     (if-exp (car cases) (list (car results)))
     (if-exp (car cases) (list (car results) (condverter (cdr cases) (cdr results)))))))))

(define let*-expand
 (lambda (defined definitions body)
  (if (null? (cdr defined))
   (syntax-expand (let-exp (list (1st defined)) (list (syntax-expand (1st definitions))) (map syntax-expand body)))
   (syntax-expand (let-exp (list (1st defined)) (list (syntax-expand (1st definitions))) (list (let*-expand (cdr defined) (cdr definitions) body))))))) 

(define syntax-expand
 (lambda (exp)
  (cases expression exp
   [let-exp (defined definitions body) (app-exp (lambda-exp defined (map syntax-expand body)) (map syntax-expand definitions))]
   [let*-exp (defined definitions body) (let*-expand defined definitions body)]
   [letrec-exp (proc-names idss bodiess letrec-bodies)
    (letrec-exp proc-names idss (map (lambda (x) (map syntax-expand x)) bodiess) (map syntax-expand letrec-bodies))]
   [namedlet-exp (name defined definitions bodies)
    (syntax-expand (letrec-exp (list name) (list defined) (list (map syntax-expand bodies)) (list (app-exp (var-exp name) (map syntax-expand definitions)))))]
   [begin-exp (bodies)
    (let [(lam (lambda-exp '() (map syntax-expand bodies)))]
     (app-exp lam '()))]
   [cond-exp (cases results)
    (condverter (map syntax-expand cases) (map syntax-expand results))]
   [while-exp (bool bodies)
    (while-exp (syntax-expand bool) (map syntax-expand bodies))]
   [app-exp (rator rands)
    (cases expression rator
     [var-exp (id)
      (case id
       [(or)
        (if (null? rands) (lit-exp #f)
         (begin
          (let [(donteval2 (de2 (car rands)))]
           (cond
            [(null? rands) (lit-exp #f)]
            [(null? (cdr rands)) (syntax-expand (if-exp (1st rands) (list donteval2 (lit-exp #f))))]
            [else (syntax-expand (if-exp (car rands) (list donteval2 (syntax-expand (app-exp (var-exp 'or) (cdr rands))))))]))))]
       [(and)
        (if (null? (cdr rands))
         (syntax-expand (1st rands))
         (if-exp (syntax-expand (1st rands)) (list (syntax-expand (app-exp (var-exp 'and) (cdr rands))) (lit-exp #f))))]
       [else (app-exp rator (map syntax-expand rands))])]
     [else (app-exp rator (map syntax-expand rands))])]
   [if-exp (condi bodies)
    (if-exp (syntax-expand condi) (map syntax-expand bodies))]
   [else exp])))

;To be added in assignment 14.

;--------------------------------------+
;                                      |
;   CONTINUATION DATATYPE and APPLY-K  |
;                                      |
;--------------------------------------+

; To be added in assignment 18a.


;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+
(define eval-bodies
 (lambda (bodies env)
  (if (null? (cdr bodies))
   (eval-exp (car bodies) env)
   (begin
    (eval-exp (car bodies) env)
    (eval-bodies (cdr bodies) env)))))

; top-level-eval evaluates a form in the global environment

(define top-level-eval
 (lambda (form)
    ; later we may add things that are not expressions.
  (eval-exp form
   (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
 (let ([identity-proc (lambda (x) x)])
  (lambda (exp env)
   (cases expression exp
    [lit-exp (datum) datum]
    [var-exp (id) (apply-env env id)]
    [lambda-exp (ids bodies) (closure ids bodies env)]
    [app-exp (rator rands)
     (let(
       [proc-value (eval-exp rator env)]
       [args (eval-rands rands env)])
      (apply-proc proc-value args))]
    [let-exp (vars exps bodies)
     (eval-bodies bodies
      (extend-env vars
       (eval-rands exps env)
       env))]
     [letrec-exp (proc-names idss bodiess letrec-bodies)
       (eval-bodies letrec-bodies
         (extend-env-recursively proc-names idss bodiess env))]
    [if-exp (condition bodies) 
     (if (eval-exp condition env)
      (eval-exp (car bodies) env)
      (if (not (null? (cdr bodies)))
       (eval-exp (2nd bodies) env)))]
    [while-exp (bool bodies)
     (cond[(eval-exp bool env) (eval-bodies bodies env) (eval-exp exp env)])]
    [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))
  

; evaluate the list of operands, putting results into a list

(define eval-rands
 (lambda (rands env)
  (map (lambda (e) (eval-exp e env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
 (lambda (proc-value args)
  (cases proc-val proc-value
   [prim-proc (op) (apply-prim-proc op args)]
   [closure (ids bodies env) (eval-bodies bodies (extend-env ids args env))]
			; You will add other cases
   [else (eopl:error 'apply-proc "Attempt to apply bad procedure: ~s" proc-value)])))

(define *prim-proc-names* '(+ list-tail eqv? - * / add1 sub1 zero? not = > >= < <= cons car cdr 
 caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list null?
 assq eq? equal? atom? length list->vector list? pair? procedure? vector->list vector
 make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline 
 quotient apply map positive? negative? append)
)

(define init-env         ; for now, our initial global environment only contains 
 (extend-env            ; procedure names.  Recall that an environment associates
  *prim-proc-names*   ;  a value (not an expression) with an identifier.
  (map prim-proc      
   *prim-proc-names*)
  (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.




(define apply-prim-proc
 (lambda (prim-proc args)
  (case prim-proc
   [(+) (apply + args)]
   [(-) (apply - args)]
   [(*) (apply * args)]
   [(/) (apply / args)]
   [(add1) (+ (1st args) 1)]
   [(sub1) (- (1st args) 1)]
   [(zero?) (zero? (1st args))]
   [(not) (not (1st args))]
   [(cons) (cons (1st args) (2nd args))]
   [(=) (= (1st args) (2nd args))]
   [(>) (> (1st args) (2nd args))]
   [(>=) (>= (1st args) (2nd args))]
   [(<) (< (1st args) (2nd args))]
   [(<=) (<= (1st args) (2nd args))]
   [(car) (car (1st args))]
   [(cdr) (cdr (1st args))]
   [(caar) (caar (1st args))]
   [(cadr) (cadr (1st args))]
   [(cdar) (cdar (1st args))]
   [(cddr) (cddr (1st args))]
   [(caaar) (caaar (1st args))]
   [(caadr) (caadr (1st args))]
   [(cadar) (cadar (1st args))]
   [(caddr) (caddr (1st args))]
   [(cdaar) (cdaar (1st args))]
   [(cdadr) (cdadr (1st args))]
   [(cddar) (cddar (1st args))]
   [(cdddr) (cdddr (1st args))]
   [(list) (apply list args)]
   [(null?) (null? (1st args))]
   [(assq) (assq (1st args) (2nd args))]
   [(eq?) (eq? (1st args) (2nd args))]
   [(eqv?) (eqv? (1st args) (2nd args))]
   [(equal?) (equal? (1st args) (2nd args))]
   [(atom?) (atom? (1st args))]
   [(length) (length (1st args))]
   [(list->vector) (list->vector (1st args))]
   [(list?) (list? (1st args))]
   [(pair?) (pair? (1st args))]
   [(procedure?)
    (if (list? (1st args))
     (or (equal? (caar args) 'prim-proc) (equal? (caar args) 'closure))
     #f)]
   [(vector->list) (vector->list (1st args))]
   [(vector) (apply vector args)]
   [(make-vector)
    (cond
     [(= (length args) 1) (make-vector (1st args))]
     [(= (length args) 2) (make-vector (1st args) (2nd args))]
     [else (eopl:error 'apply-prim-proc "make-vector has wrong arg count: ~s ~s" prim-proc args)])]
   [(vector-ref) (vector-ref (1st args) (2nd args))]
   [(vector?) (vector? (1st args))]
   [(number?) (number? (1st args))]
   [(symbol?) (symbol? (1st args))]
   [(set-car!) (set-car! (1st args) (2nd args))]
   [(set-cdr!) (set-cdr! (1st args) (2nd args))]
   [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
   [(display) (display (1st args))]
   [(newline) (newline)]
   [(quotient) (quotient (1st args) (2nd args))]
   [(apply) (apply (eval (2nd (1st args))) (2nd args))]
   [(map) (map (lambda (x) (apply-proc (1st args) (list x))) (2nd args))]
    [(list-tail) (list-tail (1st args) (2nd args))]

   [(positive?) (positive? (1st args))]
   [(negative?) (negative? (1st args))]
   [(append) (append (1st args) (2nd args))]
   [else (eopl:error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)])))

(define replcheck
 (lambda (exp)
  (cond
   [(null? exp) (list)]
   [(list? exp)
    (let [(tocheck (car exp))]
     (cond
      [(or (eqv? tocheck 'prim-proc) (eqv? tocheck 'closure)) (cons '<procedure> (replcheck (cdr exp)))]
      [else (cons (car exp) (replcheck (cdr exp)))]))]
   [else exp])))
            
       

(define rep      ; "read-eval-print" loop.
 (lambda ()
  (display "--> ")
    ;; notice that we don't save changes to the environment...
   (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently? I think I got them all
    (eopl:pretty-print (replcheck answer)) (newline)
    (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
 (lambda (exp) (eval-exp (syntax-expand(parse-exp exp)) (empty-env))))