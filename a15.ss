;austin swatek
;a15

;time saved
;original fib
;overcalculates, runs unnecessary fib(n-1). . . , spirals out of control quickly
;
;class fib-memo
;branches to end, then starts accumulating back up to fib(n). avoids unnecessary fib(n-1). . . calls
;
;this fib-memo
;creates hashtable
;looks for fib(n) in hashtable
;if fib(n) doesnt exist: calculates fib(n), stores fib(n) in hashtable, then returns fib(n)
;if fib(n) exists: looks up fib(n) in hashtable, then returns fib(n)
;having a hashtable means theres more data to store, and also means theres a hashtable lookup time.
;if the hashtable reaches capacity, it has to grow. when it grows, it has to rehash all of the values in the table.
;looking up fib(n) = more time, growing hashtable = more time, rehashing hashtable values = more time


;class stuff
(define apply-k
 (lambda (k v)
  (k v)))
(define make-k
 (lambda (v) v))

(define fact
 (lambda (n)
  (if (zero? n)
   1
   (* n (fact (- n 1))))))
(define fact-cps
 (lambda (n k)
  (if (zero? n)
   (apply-k k 1)
   (fact-cps (- n 1)
    (make-k
     (lambda (v)
      (apply-k k (* n v))))))))
;(fact-cps 6 (make-k list)) ;(720)
;(fact-cps 6 (make-k (lambda (v) (* 10 v)))) ;7200

(define list-copy-cps
 (lambda (L k)
  (if (null? L)
   (apply-k k '())
   (list-copy-cps (cdr L)
    (make-k (lambda (copied-cdr)
     (apply-k k (cons (car L) copied-cdr))))))))
;(list-copy-cps '(1 2 3) (make-k reverse)) ;(3 2 1)

(define memq-cps
 (lambda (sym lst k)
  (cond[(null? lst) (apply-k k #f)]
   [(eq? (car lst) sym) (apply-k k #t)]
    [else (memq-cps sym (cdr lst) k)])))
;(memq-cps 'a '(b c a d) (make-k list)) ;(#t)
;(memq-cps '1 '( b c d ) (make-k not)) ;#t

(define intersection
 (lambda (lst1 lst2)
  (cond[(null? lst1) '()]
   [(memq (car lst1) lst2)
    (cons (car lst1) (intersection (cdr lst1) lst2))]
    [else (intersection (cdr lst1) (lst2))])))
(define intersection-cps
 (lambda (lst1 lst2 k)
  (if (null? lst1)
   (apply-k k '())
   (intersection-cps (cdr lst1) lst2
    (make-k (lambda (cdr-intersection)
     (memq-cps (car lst1) lst2
      (make-k (lambda (is-in?)
       (apply-k k
        (if is-in?
         (cons (car lst1) cdr-intersection)
         cdr-intersection)))))))))))
;(intersection-cps '(a d e g h) '(s f c h b r a) (make-k list))

;1
(define member?-cps
 (lambda (sym lst k)
  (cond[(null? lst) (apply-k k #f)]
   [(eqv? (car lst) sym) (apply-k k #t)]
   [else (member?-cps sym (cdr lst) k)])))

(define set-of-cps
 (lambda (lst k)
  (if (null? lst)
   (apply-k k '())
   (set-of-cps (cdr lst)
    (lambda (cont)
     (member?-cps (car lst) cont
      (lambda (check)
       (if check
        (apply-k k cont)
        (apply-k k (cons (car lst) cont))))))))))

(define 1st-cps
 (lambda (lst k)
  (apply-k k (car lst))))

(define map-cps
 (lambda (proc-cps lst k)
  (if (null? lst)
   (apply-k k '())
   (map-cps proc-cps (cdr lst)
    (lambda (cont)
     (proc-cps (car lst)
      (lambda (end)
       (apply-k k (cons end cont)))))))))

(define domain-cps
 (lambda (lst k)
  (map-cps 1st-cps lst
   (lambda (domained)
    (set-of-cps domained k)))))

(define make-cps
 (lambda (proc-cps)
  (lambda (v k)
   (apply-k k (proc-cps v)))))

(define andmap-cps
 (lambda (pred-cps lst k)
  (if (null? lst)
   (apply-k k #t)
   (begin
    (pred-cps (car lst) (lambda (check)
     (if check
      (andmap-cps pred-cps (cdr lst) k)
      (apply-k k #f))))))))

(define-syntax with-values
 (syntax-rules ()
  [(_ expr consumer)
   (call-with-values
    (lambda () expr)
    consumer)]))
    
; (define fib-memo
  ; (let ([max 1]
        ; [sofar '((1 . 1) (0 . 1))])
    ; (lambda (n)
      ; (if (<= n max)
          ; (cdr (assq n sofar))
          ; (let* ([v1 (fib-memo (- n 1))]
                 ; [v2 (fib-memo (- n 2))]
                 ; [v3 (+ v2 v1)])
            ; (set! max n)
            ; (set! sofar (cons (cons n v3) sofar))
            ; v3)))))

; (define memoize
 ; (lambda (func hash equiv?)
   ; (let([calced (make-hashtable hash equiv?)])
    ; (lambda n
     ; (if (not (hashtable-ref calced n #f))
      ; (hashtable-set! calced n (apply func n)))
     ; (hashtable-ref calced n #f)))))
     
(define memoize
 (lambda (func hash equiv?)
   (let([calced (make-hashtable hash equiv?)])
    (lambda n
     (let([retVal (hashtable-ref calced n #f)])
      (if (not retVal)
       (let([newVal (apply func n)])
        (hashtable-set! calced n newVal)
        newVal)
       retVal))))))