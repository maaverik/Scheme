

(define (search bst val)
	(cond ((is-empty? bst) bst)
		((= (get-val bst) val) bst)
		((< (get-val bst) val) (search (get-rs bst) val))
		((> (get-val bst) val) (search (get-ls bst) val))
	)
)

(define (is-empty? bst) 
	(null? bst)
)

(define (get-val bst)
	(car bst)
)

(define (get-ls bst)
	(cadr bst)
)

(define (get-rs bst)
	(caddr bst)
)


(define (is-descendent? x y bst)
	(cond 
		((is-empty? bst) #f)
		((= x y) #t)
		((= (get-val bst) y) 
			(if (and (is-empty? (search (get-ls bst) x)) (is-empty? (search (get-rs bst) x) ))
					#f
					#t
			)
		)
		((< (get-val bst) y) (is-descendent? x y (get-rs bst)))
		((> (get-val bst) y) (is-descendent? x y (get-ls bst)))
		(else 'Error)
	)
)



(define (get-smallest-subtree x y bst)
	(cond
		((is-empty? bst) 'Error)
		((is-descendent? x y bst) (search bst y))
		((is-descendent? y x bst) (search bst x))
		(else 
			(cond 
				((and (is-descendent? x (get-val (get-ls bst)) bst) (is-descendent? y (get-val (get-ls bst)) bst))
					(get-smallest-subtree x y (get-ls bst)))
                ((and (is-descendent? x (get-val (get-rs bst)) bst) (is-descendent? y (get-val (get-rs bst)) bst))
					(get-smallest-subtree x y (get-rs bst)))
                (else bst)
            )
        )
     )
)

;(35 (25 (10 () ()) ()) (40 () ()))
