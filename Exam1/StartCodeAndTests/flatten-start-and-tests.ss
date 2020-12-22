(define snlist-recur
  (lambda (seed item-proc list-proc)
    (letrec ([helper
              (lambda (ls)
                (if (null? ls)
                    seed
                    (let ([c (car ls)])
                      (if (or (pair? c) (null? c))
                          (list-proc (helper c) (helper (cdr ls)))
                          (item-proc c (helper (cdr ls)))))))])
      helper)))

; add the flatten definition here. You can also substitute your snlist-recur if you wish.


; If you fail one of these tests, try a simpler slist.

(flatten '(() (a ((b ((c)) (d e (f (g)))))))) ; (a b c d e f g)

(flatten '(f () ((b d (((d) e))) () r))) ; (f b d d e r)


