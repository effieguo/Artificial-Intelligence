;Yang Guo Hw2
;04/24/2018

#lang scheme
;a helper function for swap-elements
;input: position, a list, value want to replace
;output: a list after replacement
(define (replace-nth-item pos alist val)
        (cond ((null? alist) #f)
              (#t (cond ((<= pos 0) #f)
                        ((= pos 1) (cons val (cdr alist)))
                        (#t (cons (car alist)
                                 (replace-nth-item (- pos 1) (cdr alist) val)))))))


;a helper function for swap-elements
;output: the item in the position 
(define (nth-item pos alist)
        (cond ((null? alist) #f)
              (#t (cond ((<= pos 0) #f)
                        ((= pos 1) (car alist))
                        (#t (nth-item (- pos 1) (cdr alist)))))))


;swap-elements can swap the values at two indices in a list
;input: position1, position2, a list
;output: list after swap
(define (swap-elements pos1 pos2 alist)
  (let ((num1 (nth-item pos1 alist)) (num2 (nth-item pos2 alist)))
        (replace-nth-item pos2 (replace-nth-item pos1 alist num2) num1)))


;determine if a state is adjacent to another state
;input: first state, second state
;output: #t if they are adjacent, else #f
(define (is-adjacent? state1 state2)
  ;check if adjacency-map has a sublist which first item equals to the state1
  ;if there is such sublist, return this sublist, else return #f
  (define (find-sublist state1 alist)
    (cond ((null? alist) #f)
          ((equal? state1 (car (car alist))) (car alist))
          (#t (find-sublist state1 (cdr alist)))))

  ;check if the sublist contains state2
  ;if yes, it means state1 and state2 are adjacent, return #t, else #f
  (define (contain? state2 alist)
    (cond ((null?  alist) #f)
          ((equal? state2 (car alist)) #t)
          (#t (contain? state2 (cdr alist)))))
    
  (cond ((equal? #f (find-sublist state1 adjacency-map)) #f)
        (#t (contain? state2 (cdr (find-sublist state1 adjacency-map))))))


;Helper function which can return the length of a list
;Input: a list, Output: the length of list
(define (item-counter count alist)
   (cond ((null? alist) count)
         (#t (item-counter (+ 1 count) (cdr alist)))))


;get-children can get all possible children of a state
;output: a list of lists which contains all the children of a state
(define (get-children alist)
  ;build a list with current swap positions and history swap positions
  (define (add-swap pos1 pos2 alist)
    (cond ((null? alist) (list (list pos1 pos2)))
          ((null? (cdr alist)) (append (car alist) (list (list pos1 pos2))))
          (#t (append (car alist) (add-swap pos1 pos2 (cdr alist))))))

  ;build a state list after swap between two positions
  (define (build-state pos1 pos2 alist)
     (list (swap-elements pos1 pos2 (car alist)) (add-swap pos1 pos2 (cdr alist))))             

  (define item-num (item-counter 0 (car alist)))   ;let item-num be the length of the list

  ;get all states children
  ;pos1 loop from 1 to listlength-1, and for each pos1, pos2 loop from pos1+1 to listlength
  (define (get-states pos1 pos2 alist)
     (cond ((= pos2 item-num) (cond ((= pos1 (- item-num 1)) (list (build-state pos1 pos2 alist)))
                                    (#t (cons (build-state pos1 pos2 alist)
                                              (get-states (+ 1 pos1) (+ 2 pos1) alist)))))
           (#t (cons (build-state pos1 pos2 alist) (get-states pos1 (+ 1 pos2) alist)))))
     
  (cond ((null? (car alist)) '())
         ((null? (cdr (car alist))) alist)
         (#t (get-states 1 2 alist))))


;Determine if a state is a goal state. Return #t if it is a goal state, else #f
;A goal state is a list of locations where any two locations adjacent to each other in the list
;are adjacent to each other in the map. 
(define (is-goal-state? alist)
  ;check if all neighbor states in the list are adjacent states.
  (define (goal-test alist)
    (cond ((null? alist) #f)
          ((null? (cdr alist)) #t)
          ((equal? #f (is-adjacent? (car alist) (car (cdr alist)))) #f)
          (#t (goal-test (cdr alist)))))

  (goal-test (car alist)))


;The impletation of depth-first search algorithm. There will be circle. Cannot find solution.
(define (dfs frontier)
  (cond ((null? frontier) #f)
        (#t (let ((candidate (car frontier)))
              (cond ((is-goal-state? candidate) candidate)
                    (#t (dfs (append (get-children candidate) (cdr frontier)))))))))



(define (id-dfs alist)
  ;The impletation of depth-limited DFS search algorithm.
  ;input: a cutoff number, initial edge number(edge is used to count children state's number in the frontier), a list
  (define (id-dfs-helper cutoff edge frontier)
   (cond ((null? frontier) #f)
         (#t (let ((candidate (car frontier)))
             (cond ((is-goal-state? candidate) candidate)   ;if current candidate is goal state, return this state
                   (#t (cond ((= cutoff 1)             ;if in the edge level
                              (cond ((= edge 1) (id-dfs-helper (+ 1 cutoff) edge (cdr frontier)))  ;if all the children states are checked, go to upper level, add cutoff number
                                    (#t (id-dfs-helper cutoff (- edge 1) (cdr frontier)))))        ;check next children state in the frontier
                       (#t (id-dfs-helper (- cutoff 1) (item-counter 0 (get-children candidate))   ;if not the edge level, add all children states on the front
                                          (append (get-children candidate) (cdr frontier)))))))))))  ;and go to next level, add children's number to edge

  ;try the cutoff number from 1 to the length of list - 1
  ;if there is a optimal solution, return the result immediately, else return #f 
  (define (run-id-dfs cutoff alist)
    (cond ((= cutoff (item-counter 0 alist)) #f)
          ((equal? #f (id-dfs-helper cutoff 1 alist)) (run-id-dfs (+ 1 cutoff) alist))
          (#t (id-dfs-helper cutoff 1 alist))))
          
  (cond ((is-goal-state? (list alist '())) (list alist '()))    ;if input is goal state, return 
        (#t (run-id-dfs 1 (get-children (list alist '()))))))


;The impletation of A* Algorithm.
;Heuristic: A score which determine how a state close to a goal state,
;then use this score to sort children states before add them to frontier list.
;I did it by checking each state that how many adjacent neighbors they have.
;The more adjacent states a list contains that means more the list is closer to the goal state.
(define (A* alist)
  ;determine the score of a state
  (define (find-score alist)
    (cond ((null? alist) 0)
          ((null? (cdr alist)) 0)
          ((is-adjacent? (car alist) (car (cdr alist))) (+ 1 (find-score (cdr alist))))
          (#t (find-score (cdr alist)))))

  ;sort the states list by score function, the state with larger score is on the front
  (define (sort-children alist)
    (define (compare pos alist)
      (define item-num (item-counter 0 alist))
      (cond ((= pos (+ 1 item-num)) alist)
            (#t (cond ((< (find-score (car (nth-item 1 alist))) (find-score (car (nth-item pos alist))))
                       (compare (+ 1 pos) (swap-elements 1 pos alist)))
                      (#t (compare (+ 1 pos) alist))))))

      (cond ((null? alist) '())
            ((null? (cdr alist)) alist)
            (#t (cons (car (compare 2 alist)) (sort-children (cdr (compare 2 alist)))))))

  ;similar with id-dfs-helper, just put sort-children function before get-children
  (define (A*-helper cutoff edge frontier)
   (cond ((null? frontier) #f)
         (#t (let ((candidate (car frontier)))
             (cond ((is-goal-state? candidate) candidate)
                   (#t (cond ((= cutoff 1)
                              (cond ((= edge 1) (A*-helper (+ 1 cutoff) edge (cdr frontier)))
                                    (#t (A*-helper cutoff (- edge 1) (cdr frontier)))))
                       (#t (A*-helper (- cutoff 1) (item-counter 0 (get-children candidate))
                                 (append (sort-children (get-children candidate)) (cdr frontier)))))))))))

  (define (run-A* cutoff alist)
    (cond ((= cutoff (item-counter 0 alist)) #f)
          ((equal? #f (A*-helper cutoff 1 alist)) (run-A* (+ 1 cutoff) alist))
          (#t (A*-helper cutoff 1 alist))))

  (cond ((is-goal-state? (list alist '())) (list alist '()))      ;if input is goal state
        (#t (run-A* 1 (sort-children (get-children (list alist '())))))))    

  


;Adjacency-map is a list of lists. Each of these sublists includes a location as its first item
;and all locations that are adjacent to that location as its remaining items.
(define adjacency-map '(
  (Alabama Mississippi Tennessee Georgia Florida)
  (Alaska)
  (Arkansas Texas Oklahoma Missouri Tennessee Mississippi Louisiana)
  (Arizona California Nevada Utah New-Mexico)
  (California Arizona Nevada Oregon)
  (Colorado New-Mexico Utah Wyoming Nebraska Kansas Oklahoma)
  (Connecticut New-York Massachusetts Rhode-Island)
  (Delaware Maryland Pennsylvania New-Jersey)
  (Florida Alabama Georgia)
  (Georgia Florida Alabama Tennessee North-Carolina South-Carolina)
  (Hawaii)
  (Idaho Oregon Washington Montana Wyoming Utah Nevada)
  (Indiana Illinois Michigan Ohio Kentucky)
  (Illinois Missouri Iowa Wisconsin Indiana Kentucky)
  (Iowa Missouri Illinois Wisconsin Minnesota South-Dakota Nebraska)
  (Kansas Colorado Nebraska Missouri Oklahoma)
  (Kentucky Missouri Illinois Indiana Ohio West-Virginia Virginia Tennessee)
  (Louisiana Texas Arkansas Mississippi)
  (Maine New-Hampshire)
  (Maryland Virginia West-Virginia Pennsylvania Delaware)
  (Massachusetts Rhode-Island Connecticut New-York Vermont New-Hampshire)
  (Michigan Wisconsin Indiana Ohio)
  (Minnesota North-Dakota South-Dakota Iowa Wisconsin)
  (Mississippi Louisiana Arkansas Tennessee Alabama)
  (Missouri Oklahoma Kansas Nebraska Iowa Illinois Kentucky Tennessee Arkansas)
  (Montana Idaho Wyoming South-Dakota North-Dakota)
  (Nebraska Colorado Kansas Missouri Iowa South-Dakota Wyoming)
  (Nevada California Arizona Utah Idaho Oregon)
  (New-Hampshire Maine Vermont Massachusetts)
  (New-Jersey Delaware Pennsylvania New-York)
  (New-Mexico Texas Oklahoma Colorado Arizona)
  (New-York Pennsylvania New-Jersey Connecticut Massachusetts Vermont)
  (North-Carolina South-Carolina Georgia Tennessee Virginia)
  (North-Dakota Montana South-Dakota Minnesota)
  (Ohio Michigan Indiana Kentucky West-Virginia Pennsylvania)
  (Oklahoma Texas New-Mexico Colorado Kansas Missouri Arkansas)
  (Oregon Washington Idaho Nevada California)
  (Pennsylvania Ohio West-Virginia Maryland Delaware New-Jersey New-York)
  (Rhode-Island Connecticut Massachusetts)
  (South-Carolina Georgia North-Carolina)
  (South-Dakota Nebraska Iowa Minnesota North-Dakota Montana Wyoming)
  (Tennessee Arkansas Missouri Kentucky Virginia North-Carolina Georgia Alabama Mississippi)
  (Texas New-Mexico Oklahoma Arkansas Louisiana)
  (Utah Nevada Idaho Wyoming Colorado Arizona)
  (Vermont New-York Massachusetts New-Hampshire)
  (Virginia North-Carolina Tennessee Kentucky West-Virginia Maryland)
  (Washington Oregon Idaho)
  (West-Virginia Virginia Kentucky Ohio Pennsylvania Maryland)
  (Wisconsin Minnesota Iowa Illinois Michigan)
  (Wyoming Idaho Montana South-Dakota Nebraska Colorado Utah)
))
