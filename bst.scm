(define (make-empty-bst) '())

(define (is-empty? bst) 
	(null? bst)
)

(define (make-bst val ls rs)
	(list val ls rs)
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

(define (is-leaf? bst)
	(and (is-empty? (get-ls bst)) (is-empty? (get-rs bst)))
)

(define (is-member? bst val)
	(cond
		((is-empty? bst) #f)
		((= val (get-val bst)) #t)
		((< val (get-val bst)) (is-member? (get-ls bst) val))
		((> val (get-val bst)) (is-member? (get-rs bst) val))
	)
)

(define (insert bst val)
	(cond 
		((is-empty? bst) (make-bst val (make-empty-bst) (make-empty-bst)))
		((= val (get-val bst)) bst)
		((< val (get-val bst)) (make-bst (get-val bst) (insert (get-ls bst) val) (get-rs bst)))
		((> val (get-val bst)) (make-bst (get-val bst) (insert (get-ls bst)) (get-rs bst) val))
	)
)	
		
		
(define (append ls1 ls2)
	(cond 
		((null? ls1) ls2)
		((null? ls2) ls1)
		(else (cons (car ls1) (append (cdr ls1) ls2)))
	)
)

(define (sort-bst bst)		
	(cond 
		((is-empty? bst) bst)
		(else (append (sort-bst (get-ls bst)) (list (get-val bst)) (sort-bst (get-rs bst)))
	)
)


(define (delete bst val)
	(cond 
		((is-empty? bst) (make-empty-bst))
		((< val (get-val bst)) (make-bst (get-val bst) (delete (get-ls bst) val) (get-rs bst)))
		((> val (get-val bst)) (make-bst (get-val bst) (get-ls bst) (delete (get-rs bst) val)))
		((= val (get-val bst)) (delete-root bst))
	)
)


(define (delete-root bst)
	(cond
		((is-leaf? bst) (make-empty-bst))
		((is-empty? (get-ls bst)) (get-rs bst))
		((is-empty? (get-rs bst)) (get-ls bst))
		(else (make-bst (get-val (get-leftmost (get-rs bst))) (get-ls bst) (delete (get-rs bst) (get-val (get-leftmost (get-rs bst))))))
	)
)
		


		
