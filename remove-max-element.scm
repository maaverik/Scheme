(define (find-max ls maxi)
	(cond
		((null? ls) maxi)
		((> (car ls) maxi) (find-max (cdr ls) (car ls)))
		((< (car ls) maxi) (find-max (cdr ls) maxi))
	)
)

(define (remove-max ele lst)
	(cond 
		((null? lst) lst)
		((= (car lst) ele) (cdr lst))
		((not (= (car lst) ele)) 
			(cons (car lst) (remove-max ele (cdr lst)))
		)
	)
)

(define (delete-max l)
	(remove-max (find-max l 0) l)
)
