(define (make-empty-bst)
	`()
)

(define (make-bst val ls rs)
	(list val ls rs)
)

(define (is-empty-bst? bst)
	(if (null? bst)
		#t
		#f
	)
)

(define (get-bst-val bst)
	(car bst)
)

(define (get-bst-ls bst)
	(cadr bst)
)

(define (get-bst-rs bst)
	(caddr bst)
)

(define (is-member-bst? val bst)
	(cond 	((is-empty-bst? bst)
				#f
			)
			((= val (get-bst-val bst))
				#t
			)
			((> val (get-bst-val bst))
				(is-member-bst? val (get-bst-rs bst))
			)
			(else
				(is-member-bst? val (get-bst-ls bst))
			)
	)
)

(define (bst-insert val bst)
	(cond	((is-empty-bst? bst)
				(make-bst val (make-empty-bst) (make-empty-bst))
			)
			((= val (get-bst-val bst))
				bst
			)
			((< val (get-bst-val bst))
				(make-bst (get-bst-val bst) (bst-insert val (get-bst-ls bst)) (get-bst-rs bst))
			)
			(else
				(make-bst (get-bst-val bst) (get-bst-ls bst) (bst-insert val (get-bst-rs bst)))
			)
	)
)

(define (generate-bst bst lst)
	(cond 
		((null? lst)
			'error
		)
		((null? (cdr lst))
			(bst-insert (car lst) bst)
		)
		(else
			(generate-bst (bst-insert (car lst) bst) (cdr lst))
		)
	)
)

(define (is-leaf-bst? bst)
	(and (is-empty-bst? (get-bst-rs bst)) (is-empty-bst? (get-bst-ls bst)))
)

(define (just-lesser-elm bst num)
	(cond 	((is-empty-bst? bst)
				-1
			)
			((< (get-bst-val bst) num)
				(if (> (get-bst-val bst) (just-lesser-elm (get-bst-rs bst) num))
					(get-bst-val bst)
					(just-lesser-elm (get-bst-rs bst) num)
				)
			)
			((>= (get-bst-val bst) num)
				(just-lesser-elm (get-bst-ls bst) num)
			)
	)
)

(define (just-greater-elm bst num)
	(cond	((is-empty-bst? bst)
				1000  
			)
			((> (get-bst-val bst) num)
				(if (< (just-greater-elm (get-bst-ls bst) num) (get-bst-val bst) )
					(just-greater-elm (get-bst-ls bst) num)
					(get-bst-val bst)
				)
			)
			((<= (get-bst-val bst) num)
				(just-greater-elm (get-bst-rs bst) num)
			)
	)
)

(define bst (generate-bst '() '(86	14	60	29	70	31	26	41	85	69	56	76	83	80	14	65	10	85	37	65)))
(just-lesser-elm bst 69)
