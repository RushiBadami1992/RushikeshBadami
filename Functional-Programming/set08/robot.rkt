;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;PROGRAM DESCRIPTION

;Imagine an infinite chessboard. The chessboard extends infinitely in all directions.
;You can think of the positions on the board as pairs of integers.
;On the chessboard, we have a robot and some blocks. The robot occupies a single square on
;the chessboard, as does each of the blocks. The robot can move any number of squares in
;any diagonal direction,but it can never move to or through a square occupied by a block.
;In this way, its behavior is like that of a bishop in chess.

;*****************************************************************************************
(require "extras.rkt")
(require rackunit)
;****************************************************************************************
;                                DATA DEFINITION
;*****************************************************************************************
;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated
;; direction.

;; TEMPLATE
;; move-fn move -> ??
;;(define (move-fn m)
;;       (... (direction-fn(first m))
;;            (second m))))



;;***************************************************************************************

;; A Position is a (list Integer Integer)
;; (list x y) represents the position (x,y).
;; Note: this is not to be confused with the built-in data type Posn.


;; TEMPLATE
;;(define (position-fn p)
;;       (... (first p)
;;            (second p))))


;A listOfPosition is either
; --- empty
;-- (cons position listOfPosition)

;;TEMPLATE
;;lop-fn : listOPosition->
;; define (lop-fn lstp)
;;         (cond
;;          [(empty? lstp)....]
;;         [(else (...position-fn (first lstp))
;;                   (lop-fn (rest lstp)



;; NOTE:- listOfPosition can be the list of blocks or the list of traversed list . 
;;***************:::::::::::::::::::::****************:::::::::::::::::::::::************
;; A DirectionPositionplan  is a (list direction position)

;;INTERPRETATION:
;; DirectionPositionplan is moving one step and  after each step we get a position which
;; is the given DirectionPositionplan
;;direction is one of the 4 directions .ne,se,sw,nw
;; position  represents the (x,y) co-ordinates after going specific direction indicated.

;; TEMPLATE
;;(define (directionPositionplan-fn dirpos)
;;       (...(direction-fn (first dirpos)
;;           (position-fn (second dirpos))))

;*************************************************************************************
;A listOfDirectionPositionplan is either
; --- empty
;-- (cons DirectionPositionplan listOfDirectionPositionplan)
;;INTERPRETATION:
;;A listOfDirectionPositionplan is the list of moves(DirectionPositionplan)
;;or i.e a whole plan till the target ;; ( list ( list direction posInt))

;;TEMPLATE
;;dirpos-fn : listOfDirectionPositionplan ->
;; define (dirpos-fn lstdp)
;;         (cond
;;          [(empty? lstdp)....]
;;         [(else (...directionPositionplan-fn (first lstdp))
;;                   (dirpos-fn (rest lstdp)

;************************************************************************************
;;
;; A MaybeDirectionPositionplan is either
;;     false
;;     listOfDirectionPositionplan

;; ;; TEMPLATE
;; maybeDirectionPositionplan-fn MaybeDirectionPositionplan -> ??
;; (define (maybeDirectionPositionplan-fn mdplan)
;;         (cond
;;          [(false? mdplan ....]
;;         [(else (...
;;                   (dirpos-fn mdplan)))
;;***************::::::::::::::::::::::::::::*******************::::::::::::::::**********

;; A Plan is a ListOfMove
;; WHERE: the list does not contain two consecutive moves in the same
;; direction.
;; INTERP: the moves are to be executed from the first in the list to
;; the last in the list.


;; A ListOfMove(LOM) is either:
;; empty
;; (cons Move  ListOfMove)


;; Template:-
;; plan-fn : Plan -> ??
;; (define (plan-fn plan)
;;         (cond
;;          [(empty? plan)....]
;;         [(else (...move-fn (first plan))
;;                   (plan-fn (rest plan)))


;; A Maybeplan is either
;;     false
;;     plan or ListOfMove

;; ;; TEMPLATE
;; maybeplan-fn Maybeplan -> ??
;; (define (maybeplan-fn mplan)
;;         (cond
;;          [(false? mplan ....]
;;         [(else (...
;;                   (plan-fn  mplan)))
;;***************************************************************************************
;;NOTE:- The operator is either + or -.
;;**********************:::::::::::::::::::::::*******************::::::::::::::::::::::::

(define NE "ne")
(define SE "se")
(define NW "nw")
(define SW "sw" )

;; A Direction is one of
;; -- NE or "ne"
;; -- SE or "se"
;; -- SW or "sw"
;; -- NW or "nw"


;TEMPLATE:
;direction-fn Direction -> ??
;; (define (direction-fn dir)
;;    (cond
;;    [(string=? dir NE)....]
;;    [(string=? dir NW)....]
;;    [(string=? dir SE)....]
;;    [(string=? dir SW)....]))

;;***************************************************************************************

(provide
 path
 eval-plan)

;***************************************************************************************



;************:::::::::::::::::::::************:::::::::::::::::::::**********************
;;***************************************************************************************
;;NOTE:- The operator is either + or -.
;;**********************:::::::::::::::::::::::*******************::::::::::::::::::::::::
;;CONSTANTS
(define ADD +)
(define SUBTRACT -)
(define START 0)
(define HALT 3)


;****************************************************************************************


;eval-plan : Position ListOfPosition Plan ->  MaybePosition
;GIVEN:
;1. the starting position of the robot,
;2. A list of the blocks on the board 
;3. A plan for the robot's motion
;RETURNS:
;The position of the robot at the end of executing the plan, or false
;if  the plan sends the robot to or  through any block.
;STRATEGY: Combine simpler functions.

(define (eval-plan position blocks listOfmoves)
  (final-plan position blocks listOfmoves START))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;final-plan : Position ListOfPosition Plan NonNegInt ->  MaybePosition
;; GIVEN
;;1. the starting position of the robot,
;2. A list of the blocks on the board 
;3. A plan for the robot's motion
;;4. A non negative integer.
;;RETURNS:   maybe a position after checking the scenarios if possible or will
;;           return false.
;;Strategy: USe template for ListOfMove on listOfmoves
(define (final-plan position blocks listOfmoves START)
  (cond
    [(equal? START 1) false]
    [(empty? listOfmoves) position]
    [else (walk-on-moves position blocks listOfmoves)]))


(begin-for-test
  (check-equal?
   (final-plan (list 2 5) (list 4 3) '() START)
   (list 2 5)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;walk-on-moves : Position ListOfPosition Plan ->  MaybePosition
;; GIVEN:
;;1. the starting position of the robot,
;2. A list of the blocks on the board 
;3. A plan for the robot's motion
;;RETURNS: position after checking the scenarios or will return false.
;; STRATEGY : Use HOF foldl on listOffmoves.
(define (walk-on-moves position blocks listOfmoves)
  
  (foldl
   ;move position -> Maybeposition
   (lambda (move position)
     (if (equal? false position)
         (final-plan position blocks listOfmoves 1)
         (pos-after-a-move move blocks position))) position listOfmoves))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;pos-after-a-move : Position ListOfPosition Plan ->  MaybePosition
;; GIVEN: ;1. the starting position of the robot,
;2. A list of the blocks on the board 
;3. A plan for the robot's motion
;;RETURNS:  position after checking the scenarios or will return false if block
;;encountered.
(define (pos-after-a-move move blocks position )
  (cond
    [(equal? (first move) SE)
     (traverse-robot position blocks (second move) 0  ADD SUBTRACT )]
    [(equal? (first move) SW)
     (traverse-robot position blocks (second move) 0 SUBTRACT SUBTRACT )]
    [(equal? (first move) NW)
     (traverse-robot position blocks (second move) 0 SUBTRACT  ADD)]
    [(equal? (first move) NE) 
     (traverse-robot position blocks (second move) 0  ADD ADD )]))

;****************************************************************************************

;traverse-robot : Position ListOfPosition  PosInt NonNegInt operator operator 
;;                                                                     ->  MaybePosition
;; GIVEN: ;1. the starting position of the robot,
;2. A list of the blocks on the board 
;3. A plan for the robot's motion
;4. its a halt condition which will steps through which robot will make its path .
;;5.non negative integer  initial
;;6 the operators(can be plus minus)
;;RETURNS:  position after checking the scenarios or will return false if block
;;encountered.
;;STRATEGY: divide on cases based on halt condition.

;HALTING MEASURE : when the prev (which is the initial) becomes equal to halt ie
;;             no of position robot takes.

(define ( traverse-robot position blocks halt prev op1 op2)
  (if (not(= halt prev))
      (valid-next-move-or-posn  position blocks halt prev op1 op2)
      position))


;TERMINATION AGREEMENT : The count of the prev will increase until it reaches the halt
;;measure which are the number of steps a robot will make to a particular direction.
;***************************************************************************************

;valid-next-move-or-posn : Position ListOfPosition PosInt NonNegInt operator operator 
;                                                             ->  MaybePosition
;; GIVEN: ;1. the starting position of the robot,                
;2. A list of the blocks on the board 
;3. A plan for the robot's motion
;4. steps through which robot will make its path .
;;5. initial
;;6 the operators(can be plus minus)
;;RETURNS:  position after checking the scenarios or will return false if block
;;encountered.
;;STRATEGY: divide on cases based on list of blocks matches with the next position.
(define (valid-next-move-or-posn  position blocks halt START op1 op2)
  (if (member? (next-posn position op1 op2) blocks)
      false
      (traverse-robot (next-posn position op1 op2) blocks halt (ADD START 1) op1 op2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;next-posn position operator operator -> Position
;; GIVEN: ;1. the starting position of the robot
;; 2 the operators (can be plus minus)
;;Returns: the position after applying  operators on them
;; Use template for position on position.
(define ( next-posn position op1 op2)
  (list (op1 (first position) 1)
        (op2 (second position) 1)))



(define wall1
  '((3 3)(2 3)(4 3)
         (0 5)     (4 5)
         (0 7)(2 7)(4 7))) 
(begin-for-test
  (check-equal?
   (eval-plan (list -1 1) wall1 (list (list "ne" 5)(list SE 1)(list SW 2)(list "nw" 2)))
   false))

;****************************************************************************************
;path : Position Position ListOfPosition -> MaybePlan
;GIVEN:
;1. the starting position of the robot,
;2. the target position that robot is supposed to reach
;3. A list of the blocks on the board
;RETURNS: a plan that, when executed, will take the robot from
;the starting position to the target position without passing over any 
;of the blocks, or false if no such sequence of moves exists.
;;STRATEGY: divide into cases based on the possibility of the path from source to 
;;          destination is possibe or not.

(define (path prev target blocks)
  (cond 
    [(member? prev blocks) false]
    [(member? target blocks) false] 
    [else (if (integer? (/(ADD (- (first target)(first prev))
                               (- (second target) (second prev)))
                          2))
              ;;(look-for-path prev target blocks empty (cons prev empty))
              (make-plan (look-for-path prev target blocks empty (cons prev empty)))
              false)]))


;;TESTS
;;Check test case at the end.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;make-plan  MaybeDirectionPositionplan -> Maybeplan
;;GIVEN : A maybedirectionposition list  .
;;RETURNS:  A Maybe plan which will return false or ListOfmove..
;;STRATEGY: divide on cases on lst.

(define (make-plan lst)
  (if
   (equal? lst false)
   false
   (wrapper lst)))
;***************************************************************************************           

;wrapper : listOfDirectionPositionplan -> plan
;;GIVEN: A list containing  lists of direction and position coordinate of the path.
;; RETURN-> a plan got from a list  which is a listOfMoves.
;;EXAMPLE: 
;;STRATEGY : USing HOF map on lst and  Calling simpler funcions 

(define (wrapper lst)
  (reverse(wrap
           ;;DirectionPositionplan -> move
           (map (lambda (ele) (list(first ele) 1)) 
                lst))))

;*******************************************************************************
;wrap : plan -> plan
;;GIVEN: A plan which is a list of moves(non-empty).
;; REtURN-> a plan after wrapping the list of  valid points  for every direction which
;;         is a listOfMoves.
;; EXAMPLES: check test cases for examples.
;;STRATEGY : USing template for listOfmove on lst.
(define (wrap lst)
  (cond
    [(empty? (rest lst)) (list (first lst))]
    [else (if( equal? (first (first lst))(first (second lst)))
             (wrap (cons(list (first (first lst)) (+(second (first lst)) 1))
                        (rest (rest lst))))
             (cons ( first lst)(wrap (rest lst))))]))


;***************************************************************************************
;look-for-path : Position Position ListOfPosition listOfDirectionPositionplan 
;                                       ListOfPosition -> MaybeDirectionPositionplan
;GIVEN:                                          
;1. the starting position of the robot,(prev)
;2. the target position that robot is supposed to reach(target)
;3. A list of the blocks on the board
;4 A final list which will be having direction and no oof position on the direction
;; and empty in the start.
;5 the list of position containing traversed points.
;;WHERE:
;; prev will start w.r.t traversed list which are in the contextlst .
;;CONTEXT VARIABLE
;;contextlst is the listOfDirectionPositionplan which will increase with  the number 
;; of moves from the start position till the given position .
;; traversed is the ListOfposition  which will have list of those positions which are
;; traversed from the initial till the given position.

;RETURNS:
;;a plan that, when executed, will take the robot from
;the starting position to the target position without passing over any 
;of the blocks, or false if no such sequence of moves exists.
;;STRATEGY : Use template for position .

(define (look-for-path prev target blocks contextlst traversed)
  (move-robot (choose-direction
               (-(first target)(first prev)) (-(second target )(second prev)))
              prev
              target
              blocks 
              contextlst
              traversed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;choose-direction integer integer -> direction
;;GIVEN : the x and y position of the robot.
;;RETURNS: the direction specific to the given operator condition.
;;STRATEGY: divide cases on x and y .

(define (choose-direction xcord ycord)
  (cond
    [(and ( >= xcord 0)( >= ycord 0)) NE]
    [(and ( <= xcord 0)( <= ycord 0)) SW]
    [(and ( > xcord 0)( < ycord 0)) SE]
    [(and ( < xcord 0)( > ycord 0)) NW]
    ))


(begin-for-test
  (check-equal?
   (choose-direction -1 -1) SW))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-robot: direction position position listOfposition listOfDirectionPositionplan
;;                                        listOfposition -> MaybeDirectionPositionplan
;GIVEN:
;; Direction - a  direction where robot will move.
;1. the starting position of the robot.
;2. the target position that robot is supposed to reach
;3. A list of the blocks on the board
;4 A final list which will be having direction and no oof position on the direction.
;5 the list of position containing traversed points.
;RETURN:  a maybe  DirectionPositionplan based on the condition for direciton and next move.
;STRATEGY Use template for direction on direction .
(define (move-robot direction prev target blocks contextlst traversed )
  (cond
    [(equal? prev target) contextlst]
    [(string=? direction SW)
     (next-priority-move direction prev target blocks contextlst traversed SUBTRACT SUBTRACT 0)]
    [(string=? direction NW)
     (next-priority-move direction prev target blocks contextlst traversed SUBTRACT ADD 0)]
    [(string=? direction NE)
     (next-priority-move direction prev target blocks contextlst traversed ADD ADD 0)]
    [(string=? direction SE)
     (next-priority-move direction prev target blocks contextlst traversed ADD SUBTRACT 0)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;filter-travlst ListOfPosiion position -> ListOfPosiion
;;GIVEN : A list of the points which are traversed.and position.
;;RETURNS a listofPosition after filtering out the traversed points .
;;STRATEGY: use HOF filter on travlst.

(define (filter-travlst travlst prev)
  (filter
   ;;Position -> boolean
   (lambda (entry)(not(equal? entry prev)))
   travlst))


;TEST
(check-equal?
 (filter-travlst (list(list 1 2)(list 2 3)) (list 2 3))
 (list(list 1 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;next-move position operator operator -> position
;;GIVEN : The starting position of the robot and 2 operators which will be  deciding
;; the direction of the robot.
;;RETURN : The next position of the robot after applying the operator
;;(either plus or minus) on both x and y co-ordinate  .
;;STRATEGY : Use template for position on prev.
(define (next-move prev op1 op2)
  (list(op1 (first prev) 1)
       (op2 (second prev) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; clock-dir direction -> direction
;GIVEN: Any of the direction
;RETURN : the direction after rotating it to clockwise direction.
;;STRATEGY: use template for direction on direction.

(define (clock-dir dir)
  (cond
    [(string=? dir NE)
     SE]
    [ (string=? dir SE)
      SW]
    [ (string=? dir SW)
      NW]
    [ (string=? dir NW)
      NE]
    ))

;TEST
(begin-for-test
  (check-equal? (clock-dir NW) NE)
  "did nto return northeast")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; latest-op1: direction -> Operator
;GIVEN: The direction .
;RETURNS: the operator corresponding to the particular direction.
;Strategy : use template for direction on direction.
(define (latest-op1 direction)
  (cond
    [(string=? direction NE)ADD]
    [(string=? direction SE) ADD]
    [(string=? direction SW) SUBTRACT]
    [(string=? direction NW) SUBTRACT]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; latest-op2  direction-> Operator
;GIVEN: The direction .
;RETURNS: the operator corresponding to the particular direction.
;;example (latest-op2 NE) +)
;Strategy : use template for direction on direction.
(define (latest-op2 direction)
  (cond  
    [(string=? direction NE)ADD]
    [(string=? direction SE) SUBTRACT]
    [(string=? direction SW) SUBTRACT]
    [(string=? direction NW) ADD]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;next-priority-move:direction position position ListofPosition listOfDirectionPositionplan 
;           ListofPosition operator operator NonNegint -> MaybeDirectionPositionplan
;;GIVEN
;; Direction - a  direction where robot will move.
;1. the starting position of the robot,
;2. the target position that robot is supposed to reach
;3. A list of the blocks on the board
;4 A final list which will be having direction and no oof position on the direction.
;5 the list of position containing traversed points.
;6. 2 operators
;;7 A HALT MEASURE
;;Description : If halting factor becomes greater than 3 then it will go to . 
;RETURN:  a maybe  DirectionPositionplan  for robot based on the condition for direciton
;;till the target or it will return false if block is encountered.
;;TEST :- check test case at the last.
;; STRATEGY: Divide into cases based on next position from the current(prev) in traversed
;;blocks or not.

;;HALTING MEASURE: when the steps variable reach more than 3.

(define(next-priority-move direction prev target blocks contextlst traversed op1 op2 steps)
  (cond
    [(= steps HALT)
     (path-after-traversed direction prev target blocks contextlst traversed
                           op1 op2)]
    [(or(member? (next-move prev op1 op2) traversed)
        (member?(next-move prev op1 op2) blocks))
     (next-priority-move  (clock-dir direction)  
                          prev 
                          target 
                          blocks
                          contextlst
                          traversed 
                          (latest-op1 (clock-dir direction))
                          (latest-op2 (clock-dir direction)) 
                          (ADD steps 1))]
    [else
     (look-for-path (next-move prev op1 op2)
                    target
                    blocks
                    (cons (list direction (next-move prev op1 op2)) contextlst)
                    (cons (next-move prev op1 op2) traversed))]))

;TERMINATION ARGUEMENT:At every call to next-priority-move , the halting measure 
;; increases till its limit.
;;***************************************************************************************
;path-after-traversed: position position ListofPosition listOfDirectionPositionplan
;  ListofPosition operator operator NonNegint -> MaybeDirectionPositionplan

;; Direction - a  direction where robot will move.
;1. the starting position of the robot,
;2. the target position that robot is supposed to reach
;3. A list of the blocks on the board
;4 A final list which will be having direction and no oof position on the direction.
;5 the list of position containing traversed points.
;7. 2 operators
;;RETURN:  a maybe  DirectionPositionplan  for robot based on the condition for direciton
;;till the target or it will return false if block is encountered.
;; Check test case for path function.
;;STRATEGY: cases based on position present in traverse list .

(define (path-after-traversed direction prev target blocks contextlst traversed
                              op1 op2 )
  (cond
    [(member (next-move prev ADD ADD) traversed)
     (look-for-path (next-move prev ADD ADD)
                    target
                    (cons prev blocks)
                    (cons (list NE (next-move prev ADD ADD))contextlst)
                    (filter-travlst traversed prev))] 
    [(member (next-move prev SUBTRACT SUBTRACT) traversed)
     (look-for-path (next-move prev SUBTRACT SUBTRACT)
                    target
                    (cons prev blocks) 
                    (cons (list SW (next-move prev SUBTRACT SUBTRACT))contextlst)
                    (filter-travlst traversed prev))]
    [(member (next-move prev ADD SUBTRACT) traversed)
     (look-for-path (next-move prev ADD SUBTRACT)
                    target 
                    (cons prev blocks) 
                    (cons (list SE (next-move prev ADD SUBTRACT))contextlst)
                    (filter-travlst traversed prev))]
    [(member (next-move prev SUBTRACT ADD) traversed)
     (look-for-path (next-move prev SUBTRACT ADD)
                    target
                    (cons prev blocks)
                    (cons (list NW (next-move prev SUBTRACT ADD))contextlst)
                    (filter-travlst traversed prev))]
    [else false]))


;****************************************************************************************
;TESTcases


(define wall2 
  '((0 3)(2 3)(4 3)
         (0 5)     (4 5) 
         (0 7)(2 7)(4 7)))


(define LIST1
  (list
   (list NE 1)
   (list SW 1)
   (list SE 1)
   (list NW 1)
   (list SW 2) 
   (list SE 2) 
   (list NE 1)
   (list SE 1)
   (list NE 2)
   (list NW 1)
   (list NE 1)
   (list NW 1)
   (list NE 1)
   (list NW 2)
   ))

(define LIST2
  (list
   (list NE 1)
   (list SE 1)
   (list SW 1)
   (list SE 1)
   (list SW 1)
   (list SE 2)
   (list NE 3)
   (list SW 1)
   (list SE 2)
   (list NE 4)
   (list NW 1)
   ))
(define two-walls 
  '((0 3)(4 3)
         (0 5)(4 5)
         (0 7)(4 7) 
         (0 9)(4 9)
         (0 11)(4 11)))

(define TWO-WALLS(list
 (list "ne" 1)
 (list "se" 1)
 (list "sw" 1)
 (list "se" 1)
 (list "sw" 1)
 (list "se" 2)
 (list "ne" 3)
 (list "sw" 1)
 (list "se" 2)
 (list "ne" 4)
 (list "nw" 1)))


(define  LISTRESULT (list
                     (list "ne" 1)
                     (list "sw" 1)
                     (list "se" 1)
                     (list "nw" 1)
                     (list "sw" 2)
                     (list "se" 2)
                     (list "ne" 1)
                     (list "se" 1)
                     (list "ne" 2)
                     (list "nw" 1)
                     (list "ne" 1)
                     (list "nw" 1)
                     (list "ne" 1)
                     (list "nw" 2)))

(begin-for-test
  (check-equal?
   (eval-plan (list 2 5) (rest wall2) (path (list 2 5) (list 4 9) (rest wall2)))
   (list 4 9)" didnot give the final positon after evaluating path ")
  (check-equal?
   (path (list 2 5) (list 2 6) empty)false
   " did not follow the diagnal rule")
  (check-equal?
   (path (list 2 5) (list 2 6) (list(list 2 5))) false
   "the source is the path which is not possible")
  (check-equal?
   (path (list 2 5) (list 2 6) (list(list 2 6))) false
   "did not follow black white rule")
  (check-equal?
   (eval-plan (list -3 6) two-walls LIST2)
   (list 7 6)" error in ") 
  (check-equal?
   (path (list 2 5) (list 4 9) (rest wall2)) LISTRESULT)
  (check-equal?
   (path (list -3 6) (list 7 6) two-walls) TWO-WALLS)
  (check-equal?
   (path (list 1 1)(list -1 -1) empty)
   (list (list SW 2)))
  (check-equal? (path (list 2 5) (list 4 9) wall2)
                false)

(check-equal?  (path (list 1 -1) ( list -2 2)
                          (list (list 1 0)(list 1 1)( list -1 1 )(list -1 0)(list -1 -1)))
                              WALL-NW))                    


(define WALL-NW
(list
 (list "nw" 1)
 (list "se" 1)
 (list "ne" 2)
 (list "nw" 2)
 (list "sw" 1)
 (list "nw" 1)
 (list "sw" 1)))








