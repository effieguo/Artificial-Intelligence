;AI-Hw4
;Yang Guo W1425884
;06/09/2018


;Global variables
(define nextMove 'null)
(define agents '())
(define predators '())
(define vegetations '())
(define currPercepts '())

;Initialize Agent
(define (initialize-agent) "Go!!!")

;Define all movements
(define stay "STAY")
(define turnRight "TURN-RIGHT")
(define turnLeft "TURN-LEFT")
(define turnAround "TURN-AROUND")
(define move1 "MOVE-PASSIVE-1")
(define move2 "MOVE-PASSIVE-2")
(define move3 "MOVE-PASSIVE-3")     
(define lunge1 "MOVE-AGGRESSIVE-1")
(define lunge2 "MOVE-AGGRESSIVE-2")
(define lunge3 "MOVE-AGGRESSIVE-3")
(define gulp "EAT-AGGRESSIVE")
(define eat "EAT-PASSIVE")


;API
(define (choose-action current-energy previous-events percepts)
  (begin (set! currPercepts percepts) (display previous-events)
         (cond ((equal? 'move1 nextMove) (begin (set! nextMove 'null) move1))
               ((equal? 'move2 nextMove) (begin (set! nextMove 'null) move2))
               ((equal? 'move3 nextMove) (begin (set! nextMove 'null) move3))
			   ((equal? 'turnLeft nextMove) (begin (set! nextMove 'null) turnLeft))
               ((equal? 'turnRight nextMove) (begin (set! nextMove 'null) turnRight))
               ((equal? 'turnAround nextMove) (begin (set! nextMove 'null) turnAround))
               ((attacked? previous-events)
                (cond ((and (pass? (getValue1 percepts)) (pass? (getValue2 percepts))) move2)
                      ((pass? (getValue1 percepts)) move1)
                      (#t turnRight)))
               ((not (equal? #f (vegetation? percepts))) (vegetation? percepts))
               ((not (equal? #f (predator? percepts))) (predator? percepts))
               ((not (equal? #f (agent? percepts))) (agent? percepts))
               ((pass? (getValue1 percepts)) move1)
               (#t turnRight))))



;Functions for vegetations
;==================================================================================
;Ruturn if one value is edible
(define (edible? value bloom)
  (cond ((null? value) #f)
        ((not (pair? value)) #f)
        ((not (equal? (car value) 'vegetation)) #f)
        ((> (car (cddr value)) bloom) #t)     ;if its value larger than expected value, it is worth to eat
        (#t #f)))

;Return if there is a vegetation in one move away
(define (vegeMove1 percept)
  (cond ((not (pass? (getValue1 percept))) #f)
        ((edible? (getValue2 percept) 10) #t)
        (#t #f)))

;Return if there is a vegetation in two moves away
(define (vegeMove2 percept)
  (cond ((not (pass? (getValue1 percept))) #f)
        ((not (pass? (getValue2 percept))) #f)
        ((edible? (getValue2 percept) 30) #t)
        (#t #f)))

;Return if there is a vegetation in three moves away
(define (vegeMove3 percept)
  (cond ((not (pass? (getValue1 percept))) #f)
        ((not (pass? (getValue2 percept))) #f)
        ((not (pass? (getValue3 percept))) #f)
        ((edible? (getValue4 percept) 60) #t)
        (#t #f)))

;Return the pos of vegetation if there is a vegetation in a line
(define (findVege alist pos bloom)
  (cond ((null? alist) #f)
        ((pair? (car alist))
         (cond ((and (equal? 'vegetation (caar alist)) (> (cadr (cdar alist)) bloom)) pos)
               (#t (findVege (cdr alist) (+ pos 1) bloom))))
        (#t (findVege (cdr alist) (+ pos 1) bloom))))


;Return if there are other agents attempting to eat the same vegetation
(define (fight? percept)
  (cond ((and (pair? (caar percept))
              (equal? 'agent (car (caar percept)))
              (equal? 'right (cadr (cddr (caar percept))))) #t)
        ((and (pair? (cadr (cdar percept)))
              (equal? 'agent (car (cadr (cdar percept))))
              (equal? 'left (cadr (cddr (cadr (cdar percept)))))) #t)
        ((and (pair? (car (cddr (cadr percept))))
              (equal? 'agent (caar (cddr (cadr percept))))
              (equal? 'towards (car (cddr (cdar (cddr (cadr percept))))))) #t)
        (#t #f)))


;Get the percepts
;Return the movement when we meet vegetation in different circumstance
(define (vegetation? percept)
    (cond ((not (equal? #f (findVege (car percept) 1 10))) 
           (cond ((edible? (getValue1 percept) 5)     ;if the vegetation is on the front
                  (cond ((fight? percept) gulp)       ;if there is another agent, eat-aggresive
                        (#t eat)))                    ;otherwise, eat-pressive
                 ((< (findVege (car percept) 1 10) 2) (begin (set! nextMove 'turnLeft) move1))
                 (#t (begin (set! nextMove turnRight) move1))))
          ((not (equal? #f (findVege (cadr percept) 1 30))) 
           (cond ((vegeMove1 percept) move1)          ;if the vegetation is on the 1 space away, move1
                 ((< (findVege (cadr percept) 1 30) 3) (begin (set! nextMove 'turnLeft) move2))
                 (#t (begin (set! nextMove turnRight) move2))))
          ((not (equal? #f (findVege (car (cddr percept)) 1 60)))
           (cond ((vegeMove2 percept) move2)          ;if the vegetation is on the 2 space away, move2
                 ((< (findVege (car (cddr percept)) 1 60) 4) (begin (set! nextMove 'turnLeft) move3))
                 (#t (begin (set! nextMove turnRight) move3))))
          ((not (equal? #f (findVege (cadr (cddr percept)) 1 60)))
           (cond ((vegeMove3 percept) move3)          ;if the vegetation is on the 3 space away, move3
                 (#t move1)))
          (#t #f)))


;Functions for predators
;======================================================
;Return the pos of predator if there is an predator in a line
(define (danger? alist pos)
  (cond ((null? alist) #f)
        ((pair? (car alist)) (cond ((equal? 'predator (caar alist)) pos)
                                   (#t (danger? (cdr alist) (+ pos 1)))))
        (#t (danger? (cdr alist) (+ pos 1)))))


;If there is a predator in the front of two rows, return turnAround
;If there is a predator in the front of third and fourth row, return turn left or right
(define (predator? percept)
  (cond ((not (equal? #f (danger? (car percept) 1))) (begin (set! nextMove 'move2) turnAround))
        ((not (equal? #f (danger? (cadr percept) 1))) (begin (set! nextMove 'move1) turnAround))
        ((not (equal? #f (danger? (car (cddr percept)) 1)))
         (cond ((< (danger? (car (cddr percept)) 1) 4) turnRight)
               (#t turnLeft)))
        ((not (equal? #f (danger? (cadr (cddr percept)) 1)))
         (cond ((< (danger? (cadr (cddr percept)) 1) 5) turnRight)
               (#t turnLeft)))
        (#t #f)))


;Return if my agent is being attacked
(define (attacked? event)
  (cond ((null? event) #f)
        ((not (pair? (car event))) #f)
        ((equal? 'ate (caar event))                     ;if ate, update vegetations
         (begin (set! vegetations (append vegetations (list (cdar event)))) (attacked? (cdr event)))) 
        ((equal? 'fought (caar event))                  ;if fought, update vegetations
         (begin (set! agents (append agents (list (cdar event)))) (attacked? (cdr event))))
        ((equal? 'attacked-by (caar event))             ;if be attacked, update vegetations
         (begin (set! predators (append predators (list (cdar event))))
                (cond ((< (cadr (cdar event)) -1) #t)   ;if the damage >1 means an attack
                      (#t (attacked? (cdr event))))))
        (#t (attacked? (cdr event)))))



;Functions for another agent
;=====================================================================
          
;Return the pos of agent if there is an agent in a line
(define (competitors? alist pos)
  (cond ((null? alist) #f)
        ((pair? (car alist)) (cond ((equal? 'agent (caar alist)) pos)
                                   (#t (competitors? (cdr alist) (+ pos 1)))))
        (#t (competitors? (cdr alist) (+ pos 1)))))


;If there is a agent in the front of three rows, turn left or right
(define (agent? percept)
  (cond ((not (equal? #f (competitors? (car percept) 1)))
         (cond ((< (competitors? (car percept) 1) 2) turnRight)
               (#t turnLeft)))
        ((not (equal? #f (competitors? (cadr percept) 1)))
         (cond ((< (competitors? (cadr percept) 1) 3) turnRight)
               (#t turnLeft)))
        ((not (equal? #f (competitors? (car (cddr percept)) 1)))
         (cond ((< (competitors? (car (cddr percept)) 1) 4) turnRight)
               (#t turnLeft)))
        ;((not (equal? #f (competitors? (cadr (cddr percept)) 1)))
         ;(cond ((< (competitors? (cadr (cddr percept)) 1) 5) turnRight)
         ;      (#t turnLeft)))
        (#t #f)))



;Basic Helper Functions
;=====================================================================
;Return if the space is passable
(define (pass? loc)
  (cond ((equal? loc 'empty) #t)
        (#t #f)))


;Returns the first value in front of the agent
(define (getValue1 percept)
	(cadr (car percept)))


;Returns the second value in front of the agent
(define (getValue2 percept)
	(cadr (cdar (cdr percept))))


;Returns the third value in front of the agent
(define (getValue3 percept)
	(cadr (cddr (cadr (cdr percept)))))


;Returns the fourth value in front of the agent
(define (getValue4 percept)
	(cadr (cddr (cdar (cddr (cdr percept))))))

        
