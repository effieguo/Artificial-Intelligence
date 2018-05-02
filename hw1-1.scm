;;;Written by Yang Guo
;;03/2018


;;;Basic Assignment
;Takes a list comprised of numerical values and returns the largest one.
(define (find-biggest alist)
        (cond ((null? alist) #f)
              ((null? (cdr alist)) (car alist))
              ((< (car alist) (car (cdr alist))) (find-biggest (cdr alist)))
              (#t (find-biggest (cons (car alist) (cdr (cdr alist)))))))
 
       

;Takes two integer parameters: a starting and an ending value. Prints all integers between those two number (inclusive)
(define (count-from a b)
        (cond ((> a b) #f)
              ((= a b) (display a) (newline))
              (#t (begin (display a) (newline) (count-from (+ a 1) b)))))



;Takes two parameters: an index (position) and a list. Returns the item in the list corresponding to the index specified
(define (nth-item pos alist)
        (cond ((null? alist) #f)
              (#t (cond ((<= pos 0) #f)
                        ((= pos 1) (car alist))
                        (#t (nth-item (- pos 1) (cdr alist)))))))



;Takes three parameters: an index (position), a list, and a value to substitute into the list. Returns the list, with the item at the specified index replaced with the new value
(define (replace-nth-item pos alist val)
        (cond ((null? alist) #f)
              (#t (cond ((<= pos 0) #f)
                        ((= pos 1) (cons val (cdr alist)))
                        (#t (cons (car alist)
                                    (replace-nth-item (- pos 1) (cdr alist) val)))))))



;Takes a single parameter: a list. Returns a boolean value indicating if that list is in a (numerically) sorted order.
(define (sorted? alist)
        (cond ((null? alist) #f)
              ((null? (cdr alist)) #t)
              ((> (car alist) (car (cdr alist))) #f)
              (#t (sorted? (cdr alist)))))



;Takes two parameters: an action and a state. Returns the updated state that will result by following that action. A state is a list of three elements: an X position, a Y position, and a direction (N, S, E, or W).  An action is one of the following strings: STAY, MOVE-1, MOVE-2, MOVE-3, TURN-LEFT, TURN-RIGHT, or TURN- AROUND. Assume that all moves are forward, relative to the current direction.
(define (apply-action st act)
        (cond ((equal? act "STAY") st)
              ((equal? act "MOVE-1") (cond ((equal? (list 'N) (cddr st)) (list (car st) (+ (cadr st) 1) 'N))
                                           ((equal? (list 'S) (cddr st)) (list (car st) (- (cadr st) 1) 'S))
                                           ((equal? (list 'W) (cddr st)) (cons (- (car st) 1) (cdr st)))
                                           (#t (cons (+ (car st) 1) (cdr st)))))
              ((equal? act "MOVE-2") (cond ((equal? (list 'N) (cddr st)) (list (car st) (+ (cadr st) 2) 'N))
                                           ((equal? (list 'S) (cddr st)) (list (car st) (- (cadr st) 2) 'S))
                                           ((equal? (list 'W) (cddr st)) (cons (- (car st) 2) (cdr st)))
                                           (#t (cons (+ (car st) 2) (cdr st)))))
              ((equal? act "MOVE-3") (cond ((equal? (list 'N) (cddr st)) (list (car st) (+ (cadr st) 3) 'N))
                                           ((equal? (list 'S) (cddr st)) (list (car st) (- (cadr st) 3) 'S))
                                           ((equal? (list 'W) (cddr st)) (cons (- (car st) 3) (cdr st)))
                                           (#t (cons (+ (car st) 3) (cdr st)))))
              ((equal? act "TURN-LEFT") (cond ((equal? (list 'N) (cddr st)) (list (car st) (cadr st) 'W))
                                              ((equal? (list 'S) (cddr st)) (list (car st) (cadr st) 'E))
                                              ((equal? (list 'W) (cddr st)) (list (car st) (cadr st) 'S))
                                              (#t (list (car st) (cadr st) 'N))))
              ((equal? act "TURN-RIGHT") (cond ((equal? (list 'N) (cddr st)) (list (car st) (cadr st) 'E))
                                               ((equal? (list 'S) (cddr st)) (list (car st) (cadr st) 'W))
                                               ((equal? (list 'W) (cddr st)) (list (car st) (cadr st) 'N))
                                               (#t (list (car st) (cadr st) 'S))))
              ((equal? act "TURN-AROUND") (cond ((equal? (list 'N) (cddr st)) (list (car st) (cadr st) 'S))
                                                ((equal? (list 'S) (cddr st)) (list (car st) (cadr st) 'N))
                                                ((equal? (list 'W) (cddr st)) (list (car st) (cadr st) 'E))
                                                (#t (list (car st) (cadr st) 'W))))
              (else #f)))



;;;Advanced Assignment
;Takes three parameters: a percept (a la AgentWorld), an X coordinate, and a Y coordinate. Assuming that the agent is in location 0,0 and is facing north, returns the element in the specified location.
(define (get-location percept x y)
        (nth-item (+ x y 1) (nth-item y percept)))



;;;Extra Credit
;Takes two lists of integers, each assumed to be in ascending order. Returns a single list comprised of the values from both lists in ascending order.
(define (merge-ordered-lists list1 list2)
        (cond ((null? list1) list2)
	      ((null? list2) list1)
              ((< (car list1) (car list2))
			(cons (car list1) (merge-ordered-lists (cdr list1) list2)))
	      (#t (cons (car list2) (merge-ordered-lists (cdr list2) list1)))))



;Takes one parameter, a list of integer values. Returns a list with the same values, sorted in increasing  numerical order. Merge sort will operate by splitting the list in half (+/- 1 element), sorting each of those  lists (recursively,using merge-sort), and then merging the two sorted lists
(define (merge-sort alist)
	(cond ((null? alist) '())
	      ((null? (cdr alist)) alist)
              (#t (let ((lists (split alist)))
		            (let ((list1 (car lists)) (list2 (car (cdr lists))))
				(merge-ordered-lists (merge-sort list1) (merge-sort list2)))))))



;Helper function for merge-sort to split the list
(define (split alist)
		(cond ((null? alist) (list '() '()))
		      ((null? (cdr alist)) (list alist '()))
		      (#t (let ((restl (split (cdr (cdr alist)))))
			          (list (cons (car alist) (car restl))
					(cons (car (cdr alist)) (car (cdr restl))))))))



