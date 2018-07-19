;;Yang Guo
;05/10/2018



;A helper function,which gets a list and a value
;return #t if the list contains this value, return #f otherwise
(define (contain? alist val)
    (cond ((null? alist) #f)
          ((equal? val (car alist)) #t)
          (#t (contain? (cdr alist) val))))


;A statement resolution algorithm. It should accept statements in an abbreviated form of conjunctive normal form (CNF), where each argument is a list of elements that are implicitly or’d together (a disjunction of variables).
;If the two statements can resolve, it will return the resolution.
;If the two statements cannot be resolved, it will return #f.
;If the two statements resolve to a contradiction, it should return CONTRADICTION.
(define (resolve list1 list2)

  ;return #t if two lists can be resolved, return #f otherwise
  ;num to contain how many variables can be resolved
  (define (resolvable? list1 list2 num)
    (cond ((null? list1) (cond ((= num 1) #t)   ;if num equals to 1, means two lists are resolvable
                               (#t #f)))
          ((list? (car list1)) (cond ((contain? list2 (cadr (car list1))) (resolvable? (cdr list1) list2 (+ 1 num)))
                                     (#t (resolvable? (cdr list1) list2 num))))
          (#t (cond ((contain? list2 (list 'NOT (car list1))) (resolvable? (cdr list1) list2 (+ 1 num)))
                    (#t (resolvable? (cdr list1) list2 num))))))
 
  ;return #t if two lists are contradiciton, return #f otherwise
  (define (contradiction? list1 list2)
    (cond ((null? (cdr list1))
           (cond ((null? (cdr list2))
                  (cond ((equal? (list 'NOT (car list1)) (car list2)) #t)
                        ((equal? (list 'NOT (car list2)) (car list1)) #t)
                        (#t #f)))
                 (#t #f)))
          (#t #f)))

  ;return a new list by removing a specific variable from original list
  (define (listExceptOne alist val)
    (cond ((null? alist) '())
          ((equal? val (car alist)) (listExceptOne (cdr alist) val))
          (#t (cons (car alist) (listExceptOne (cdr alist) val)))))

  ;resolve two lists
  (define (resolvelists list1 list2)
    (cond ((null? list1) list2)
          ((list? (car list1)) (cond ((contain? list2 (cadr (car list1)))   ;found the variable can be resolved
                                      (resolvelists (cdr list1) (listExceptOne list2 (cadr (car list1))))) 
                                     ((contain? list2 (car list1)) (resolvelists (cdr list1) list2))  ;avoid duplicates
                                     (#t (cons (car list1) (resolvelists (cdr list1) list2)))))
          (#t (cond ((contain? list2 (list 'NOT (car list1)))
                     (resolvelists (cdr list1) (listExceptOne list2 (list 'NOT (car list1)))))
                    ((contain? list2 (car list1)) (resolvelists (cdr list1) list2))
                    (#t (cons (car list1) (resolvelists (cdr list1) list2)))))))
     
  
  (cond ((equal? #f (resolvable? list1 list2 0)) #f)     ;if lists cannot be resolved, return #f
        ((contradiction? list1 list2) 'CONTRADICTION)    ;if lists are contradiction
        (#t (resolvelists list1 list2))))                



;A propositional logic solver that accepts statements in CNF and then performs resolution to answer propositional logic questions. For this assignment, your solver should implement a tell and ask interface on top of the resolver from the basic assignment, using the same syntax for disjunctive statements.

;A global variable, containing all lists in knowledge base
(define KB '())

;Add an statement into knowledge base
(define (tell alist)
  (cond ((null? KB) (begin (set! KB (list alist)) 'OK))   ;if KB is null
        ((contain? KB alist) 'OK)    ;if this statement is already in the KB
        (#t (begin (set! KB (append KB (list alist))) 'OK))))    ;append a new one in KB

;The ask function return #t for a statement that can be inferred, or UNKNOWN otherwise.
(define (ask alist)

  ;Given a statement and a list of statement, this state make resolution with each of state in the list
  ;if a contradicion is found, return #t
  ;otherwise return a list of all results of resolution
  (define (resolveStates state alist)
    (cond ((null? alist) '())
          ((equal? 'CONTRADICTION (resolve state (car alist))) #t)
          ((equal? #f (resolve state (car alist))) (resolveStates state (cdr alist)))
          (#t (cond ((equal? #t (resolveStates state (cdr alist))) #t)
                    (#t (cons (resolve state (car alist)) (resolveStates state (cdr alist))))))))

  ;put the first statement of kb, and the rest statements of kb into resolvesStates
  (define (helper kb)
    (cond ((null? kb) 'UNKNOWN)   ;if there is no more resolution, return UNKNOWN
          ((null? (cdr kb)) 'UNKNOWN)
          ((equal? #t (resolveStates (car kb) (cdr kb))) #t) 
          (#t (helper (append (cdr kb) (resolveStates (car kb) (cdr kb)))))))

  (cond ((contain? KB alist) #t)    ;if the KB contains this statement
        (#t (helper (append KB (list (list (cons 'NOT alist))))))))
             ;add the negation of the statement in kb, pass to helper
           


