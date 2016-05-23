

(define (comparedate d1 d2)
		(if (and (= (car d1) (car d2)) (= (cadr d1) (cadr d2)) (= (caddr d1) (caddr d2))) 
			#t
			#f
		)
)




(define (validlist lst)
		(define (checkdate d)
			(if (and (not (null? d)) (not (null? (cdr d))) (not (null? (cddr d))))
				(if (and (<= (car d) 31) (<= (cadr d) 12) (<= (caddr d) 2014))     
					#t
					#f
				)
			)
		)
		
		(cond
			((null? lst) '())
			((checkdate (cadar lst)) (cons (car lst) (validlist (cdr lst))))
			(else (validlist (cdr lst)))
		)
)
		
(define (removeduplicates lst date)
		(cond
			((null? lst) '())
			((comparedate (cadar lst) date) (removeduplicates (cdr lst) date))
			(else (cons (car lst) (removeduplicates (cdr lst) date)))
		)
)


 
(define (main mainlst)
		(define (outerloop lst1)

				(define (innerloop lst2)
					(cond
						((null? lst2) '())
						((comparedate (cadr (car lst1)) (cadr (car lst2))) 
							(cons (caar lst2) (innerloop (cdr lst2))))
						(else (innerloop (cdr lst2)))
					)
				)

				(cond
					((null? lst1) '())
					((not (null? (innerloop (cdr lst1))))
						(append (list (cons (caar lst1) (innerloop (cdr lst1)))) (outerloop (removeduplicates (cdr 								lst1) (cadar lst1)))))
					(else (outerloop (cdr lst1)))
					
					
				)
		

			
			
		)	
		(outerloop (validlist mainlst))
)




;'(("name1" (1 2 3)) ("name2" (31 12 2010)) ("name3" (37 9 2011)) ("name4" (1 2 3)) ("name5" (2 3 4)) ("name6" (2 3 4)))  
