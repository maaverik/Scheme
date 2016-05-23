(define (compare-time time1 time2)
	(cond 
		((null? time1) #f)
		((null? time2) #t)
		((< (time-interval time1) (time-interval time2)) #t)
		(else #f)
	)
)


(define (time-list lst)
	(cond
		((null? (cddr lst)) lst)
		(else (time-list (cdr lst)))
	)
)


(define (time-interval time-lst)
	(define m (-(car (cdadr time-lst)) (cadar time-lst)))
	(define h (- (caadr time-lst) (caar time-lst)))
	(cond
		((and (>= m 0) (>= h 0)) (+ (* h 60) m))
		(else  (+ (* (- h 1) 60) m 60))
			
	)
)



(define (valid-list ls)
	(cond
		((null? (car ls)) (list (cdr ls)))
		((check-time (cddar (car ls)) (cadr ls))  (cons (cons (caar ls) (car (valid-list (cons (cdar ls) (cdr ls))))) (cdr (valid-list (cons (cdar ls) (cdr ls))))))
		(else (valid-list (cons (cdar ls) (cdr ls))))
	)
)



(define (check-time train-time time-ls)
	(cond
		((and (> (caar train-time) (caar time-ls)) (< (caadr train-time) (caadr time-ls))) #t)
		((and (= (caar train-time) (caar time-ls)) (= (caadr train-time) (caadr time-ls)) (>= (cadar train-time) (cadar time-ls)) (<= 				(cadar (cdr train-time)) (cadar (cdr time-ls)))) #t)
		((and (= (caar train-time) (caar time-ls)) (>= (cadar train-time) (cadar time-ls)) (< (caadr train-time) (caadr time-ls))) #t)
		((and (= (caadr train-time) (caadr time-ls)) (<= (cadar (cdr train-time)) (cadar (cdr time-ls))) (> (caar train-time) (caar 				time-ls))) #t)
		(else #f)
	)
)	
		
			


(define (min-record lst)
	(cond 	((null? (cddar (car lst))) '("No valid train"))
		((null? (cdar lst)) (caar lst))
		((compare-time (time-list (caar lst)) (time-list (cadar lst))) (min-record (cons (cons (caar lst) (cddar lst)) (cdr lst))))
		(else (min-record  (cons (cdar lst) (cdr lst))))
	)
)





(define (main lst)
	(cond 
		(	(or 	(null? (cdr lst))
				(null? lst)
			)
						'error
		)
		(else (car (min-record (valid-list lst))))
	)
)
		


;(main '( ( (12 "fsd" (12 23) (23 23)) (12 "dsfs" (22 34) (23 12)) ) ((12 34) (24 33))))
;(main '( ( (12 "fsd" (12 23) (23 23)) (12 "dsfs" (22 34) (23 12)) (11 "dfsd" (21 21) (23 23)) (10 "sdfds" (12 20) (21 21)) ) ( (12 00) (24 00) ) ) )
