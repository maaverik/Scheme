(define (find-max ls)
	(cond
		((null? ls) ls)
		((>= (car ls) (cadr ls)) (find-max (cons (car ls) (cddr ls)))
		((< (car ls) maxi) (find-max (cdr ls)))
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
	(remove-max (find-max l) l)
)
