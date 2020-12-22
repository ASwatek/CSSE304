; paste your bt-max-interior code here and nodify it to meet the new specification.



(bt-max-interior
  '(a 2 (c (d -1 -2) (e -1 -2)))) ; e


(bt-max-interior
 '(a 0 (b 7 (c (d -2 -1) (e -3 -4))))) ; d

(bt-max-interior
 '(a (b (c (d -2 -1) (e -3 -4)) 7) 0)) ; a

(bt-max-interior
 '(a 0 (b 2 3))) ; b

(bt-max-interior '(a (b 2 3) 0)) ; a

(bt-max-interior '(a 2 (b (c -1 -1) (d -1 -1)))) ; d 

(bt-max-interior '(a (b (c -1 -1) (d -1 -1)) 2)) ; a

(bt-max-interior '(a (b -2 (d -1 -1)) (c (e -1 -1) -2))) ; e
					;








