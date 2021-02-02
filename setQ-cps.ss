(define set?-cps
 (lambda (lst k)
  (if (null? lst)
   (apply-k k #t)
   (member?-cps (car lst) (cdr lst)
    (lambda (check)
     (if check
      (apply-k k #f)
      (set?-cps (cdr lst) k)))))))