(define (make-ecom-db)
	list)
(define (get-first-dept ecom-db)
	(car ecom-db)
)
(define (get-rest-dept ecom-db)
	(cdr ecom-db)
)

(define (make-dept-record dept-name prod-rec)
	(list dept-name prod-rec)
)
(define (get-dept-name dept-record)
	(car dept-record)
)
(define (get-prod-rec dept-record)
	(cadr dept-record)
)

(define (make-prod-rec)
	list
)
(define (get-first-prod-record prod-rec)
	(car prod-rec)
)
(define (get-rest-prod-record prod-rec)
	(cdr prod-rec)
)

(define (make-prod-record pid manufacturer cost)
	(list pid manufacturer cost)
)
(define (get-pid prod-record)
	(car prod-record)
)
(define (get-manufacturer prod-record)
	(cadr prod-record)
)
(define (get-cost prod-record)
	(caddr prod-record)
)

(define (main ecom-db dept-name manufacturer)
	(minimum (get-required-prod-recs (get-required-dept-prod-recs ecom-db dept-name) manufacturer) )
)

(define (get-required-prod-recs prod-rec manufacturer)
	(cond 
		((null? prod-rec) '())
		((equal? (get-manufacturer (get-first-prod-record prod-rec))) manufacturer)
			(cons (get-first-prod-record prod-rec) (get-required-prod-recs (get-rest-product-record prod-rec) manufacturer)))
		(else (get-required-prod-recs (get-rest-product-record prod-rec) manufacturer))
	)
)

(define (get-required-dept-prod-recs ecom-db dept-name)
	(cond 
		((null? ecom-db) '())
		((equal? (get-dept-name (get-first-dept ecom-db))) dept-name)
			(cons (get-prod-rec (get-first-dept ecom-db))  (get-required-dept-prod-recs (get-rest-dept ecom-db) dept-name)))
		(else (get-required-dept-prod-recs (get-rest-dept ecom-db) dept-name))
	)
)

(define (minimum prod-rec )
	(cond 
		((null? prod-rec) 'ERROR)	
		((null? (get-rest-prod-record prod-rec)) (get-first-prod-record prod-rec))
		(( < (get-cost (get-first-prod-record prod-rec)) (get-cost (get-first-prod-record (get-rest-prod-record prod-rec))))
			(minimum (cons (get-first-prod-record prod-rec) (get-rest-prod-record (get-rest-prod-record prod-rec)))))
		(else (minimum (get-rest-prod-record prod-rec)))
	)
)






;( ( "dname1"  ( ("pid11" "man11" 11) ("pid12" "man12" 12) ("pid13" "man13" 13) ) ) ( "dname2" ( ("pid21" "man21" 21) ("pid22" "man22" 22) ("pid23" "man23" 23) ) ) ( "dname3"  ( ("pid31" "man31" 31) ("pid32" "man32" 32) ("pid33" "man33" 33) ) ) )
