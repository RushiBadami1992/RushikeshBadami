#lang racket


(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(provide make-world
         run
         make-square-toy
         make-throbber
         make-clock
         make-football
         Toy<%>
         PlaygroundState<%>)
(check-location "09" "toys.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT ))
(define SQUARE-SIDE 40)
(define THROBBER-MIN-RADIUS 5)
(define THROBBER-MAX-RADIUS 20)
(define HALF-SQUARE-WIDTH 20)
(define RIGHT-BOUNDARY 480)
(define TWO 2)
(define TICK 1)
(define ZERO 0)

(define TARGET-RADIUS 10)
(define TWELVE 12)
(define FOOTBALL (bitmap/file "football.png"))
(define HALF-FOOT-WIDTH  (/ (image-width  FOOTBALL) 2))
(define HALF-FOOT-HEIGHT (/ (image-height FOOTBALL) 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Color Constants
(define BLUE "blue")
(define GREEN "green")
(define BLACK "black")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY-EVENTS
(define SQUARE "s")
(define THROBBER "t")
(define CLOCK  "w")
(define BALL "f")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOUSE-EVENT
(define BUTTON-DOWN "button-down")
(define BUTTON-UP  "button-up")
(define DRAG "drag")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DESIGN

#|
a ListOfToy(LOT) is a list which is either:
-- empty
-- (cons Toy LOT)

lot-fn: LOT -> ??
(define (lot-fn lot)
    (cond
        [(empty?) ...]
        [else ... (first lot)
              ... (lot-fn (rest lot))]))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACEs

;; Toys and Target implement Widget<%>
;; interface.

(define Widget<%>
  (interface ()
    
    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          
    
    ; PosInt PosInt ->
    
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    
    ; KeyEvent -> Widget
    ; GIVEN: a key event
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     
    
    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The World implements the WorldState<%> interface
(define WorldState<%>
  (interface ()
    
    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          
    
    ; PosInt PosInt MouseEvent-> World
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event
    
    ; KeyEvent -> WorldState
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event
    
    ; -> Scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Playground with an target in it
(define PlaygroundState<%>
  (interface (WorldState<%>)
    
    ;; -> Integer
    ;; RETURN: the x and y coordinates of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; Is the target selected?
    target-selected?
    
    ;; -> ListOfToy
    get-toys
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Toy<%> 
  (interface (Widget<%>)
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a square, it is the velocity of the square (rightward is
    ;; positive)
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a football, it is the current size of the football (in
    ;; arbitrary units; bigger is more)
    toy-data))

;; make-Playground: ListOfToy Target PosInt-> Playground
;; GIVEN: a ListOfToy, a Target and the PosInt as travel speed
;; RETURNS: a WorldState initialized with the above parmeters
;; STRATEGY: call simpler function
(define (make-Playground objs  target speed)
  (new Playground% [objs objs]  [target target] [speed speed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Playground% class
;; ;; A Playground is a(new Playground% [objs LOT][target Target][speed Integer])

;; Playground consist of toys
(define Playground%
  (class* object% (PlaygroundState<%>)
    ;;This field is a list of toy objects
    (init-field objs) ;  ListOfWidget
    ;;These init-field is you to initalize target and speed with which rectangle should travel 
    (init-field target speed)
    
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; target-x: ->PosInt
    ;; Returns:The x coordinate of target
    ;; Example:Check test at end of program for example.
    ;; Design-Strategy:Combine Simpler Functions.
    
    (define/public (target-x)
      (dynamic-get-field 'x target))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; target-y: ->PosInt
    ;; Returns:The y coordinate of target
    ;; Example:Check test at end of program for example.
    ;; Design-Strategy:Combine Simpler Functions.    
    (define/public (target-y)
      (dynamic-get-field 'y target))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; target-selected?: ->PosInt
    ;; Returns:True iff target is selected.
    ;; Example:Check test at end of program for example.
    ;; Design-Strategy:Combine Simpler Functions.    
    
    (define/public (target-selected?)
      (dynamic-get-field 'selected? target))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-tick: ->Playground
    ;; Returns:A playground object.
    ;; Example:Check test at end of program for example.
    ;; Design-Stratergy:Use HOF Map on LOT.    
    (define/public (after-tick)
      (make-Playground
       (map
        
        (lambda (obj) (send obj after-tick))
        objs)
       target speed))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    ;; to-scene : -> Scene
    ;; RETURNS:A scene with all toys placed on it
    ;; Example:Check test at end of program for example.   
    ;; DESIGN-STRATERGY:Use HOFC foldr on the Widget's in this World
    (define/public (to-scene)
      (foldr
       (lambda (obj scene)
         (send obj add-to-scene scene))
       (send target add-to-scene EMPTY-CANVAS)
       objs))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-key-event : Keyevent -> Playground
    ;; RETURNS:A playground after key events.   
    ;; STRATEGY: Cases on kev
    ;; "b" and "h" create new bomb and new helicopter;
    ;; other keystrokes are passed on to the objects in the world.
    (define/public (after-key-event kev)
      (cond
        [(key=? kev SQUARE)
         (make-Playground
          (cons (make-square-toy (target-x) (target-y) speed) objs)
          target speed)]
        [(key=? kev THROBBER)
         (make-Playground
          (cons (make-throbber (target-x) (target-y)) objs)
          target speed)]
        [(key=? kev CLOCK)
         (make-Playground
          (cons (make-clock (target-x) (target-y)) objs)
          target speed)]
        [(key=? kev BALL)
         (make-Playground
          (cons (make-football (target-x) (target-y)) objs)
          target speed)]
        [else
         (make-Playground
          (map
           (lambda (obj) (send obj after-key-event kev))
           objs)
          target speed)]))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; world-after-mouse-event : PosInt PosInt MouseEvent -> Playground
    ;; GIVEN:X and Y coordinates of mouse.
    ;; RETURNS:The Playground after applying mouse event
    ;; EXAMPLE:Check test for example.    
    ;; STRATEGY: Cases on mouse event
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (world-after-button-down mx my)]
        [(mouse=? mev "drag")
         (world-after-drag mx my)]
        [(mouse=? mev "button-up")
         (world-after-button-up mx my)]
        [else this]))
    
    ;; the next few functions are local functions, not in the interface.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; world-after-button-down:PosInt PosInt -> Playground
    ;; GIVEN:X and Y coordinates of current position of mouse.
    ;; RETURNS:The playground after mouse down event.
    ;; EXAMPLE:Check test for example.
    ;; STRATEGY:Use HOF map on LOT    
    (define (world-after-button-down cur-mx cur-my)
      (make-Playground
       (map
        (lambda (obj) (send obj after-button-down cur-mx cur-my))
        objs)
       (send target after-button-down cur-mx cur-my) speed))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; world-after-button-up:PosInt PosInt -> Playground
    ;; GIVEN:X and Y coordinates of current position of mouse.
    ;; RETURNS:The playground after mouse up event.
    ;; EXAMPLE:Check test for example.
    ;; STRATEGY:Use HOF map on LOT    
    
    (define (world-after-button-up cur-mx cur-my)
      (make-Playground
       (map
        (lambda (obj) (send obj after-button-up cur-mx cur-my))
        objs)
       (send target after-button-up cur-mx cur-my) speed))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; world-after-drag:PosInt PosInt -> Playground
    ;; GIVEN:X and Y coordinates of current position of mouse.
    ;; RETURNS:The playground when image is getting dragged.
    ;; EXAMPLE:Check test for example.
    ;; STRATEGY:Use HOF map on LOT    
    
    (define (world-after-drag current-mx current-my)
      (make-Playground
       (map
        (lambda (obj) (send obj after-drag current-mx current-my))
        objs)
       (send target after-drag current-mx current-my) speed))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-toys:->LOT
    ;; RETURNS:A list of toys as object.
    ;; Example:Check test for example
    ;; Design-Stratergy:Combine Simpler Function    
    (define/public (get-toys)
      objs) 
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We have four classes  of Toy<%>:Square,Throbber,Clock,Football

;; A square moves from right to left along x axis with given speed
;; A Square is a(new Square% [x PosInt] [y PosInt][speed Integer][selected? Boolean]
;;                           [saved-mx Integer][saved-my Integer])
;; A square is draggable toy.  
(define Square%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one square toy to
    ;; the next.
    
    ; the x and y position of the center of the square and speed is the speed with which
    ;;the square should move.
    (init-field x y speed)   
    
    ; is the square toy selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the square is selected, the position of
    ;; the last button-down event inside the square toy, relative to the
    ;; square's center.  Else any value.
    (init-field [saved-mx ZERO] [saved-my ZERO])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    ; the square toys side
    (field [s SQUARE-SIDE])   
    
    ; image for displaying the square toy
    (field [SQUARE-IMG (square s "outline" BLUE)])
    
    
    
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; toy-x:->PosInt
    ;; RETURNS:The x coordinate of the toy.
    ;; Example:Check test for example.    
    ;; DESIGN-STRATERGY:Combine Simpler Functions    
    (define/public (toy-x)
      x)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; toy-y: ->PosInt
    ;; RETURNS:The y coordinate of the toy.
    ;; Example:Check test for example    
    ;; DESIGN-STRATERGY:Combine Simpler Functions     
    (define/public (toy-y)
      y)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; toy-data: ->PosInt
    ;; RETURNS:The speed of the toy.
    ;; Example:Check test for example    
    ;; DESIGN-STRATERGY:Combine Simpler Functions   
    (define/public (toy-data)
      speed)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-tick : -> Square
    ;; RETURNS: A square  like this one, but as it should be after a tick
    ;; a selected square doesn't move.
    ;; EXAMPLE:Check Test for example.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (new Square%
               [x (check-for-collision x speed)]
               [y  y]
               [speed (change-velocity speed x)]
               [selected? selected?]
               [saved-mx saved-mx]
               [saved-my saved-my])))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; check-for-collision:PosInt Int -> PosInt
    ;; GIVEN:Speed with which the rectangle is moving and x-coordinate of rectangle.
    ;; RETURNS:The X coordinate of the rectangle for current clock tick
    ;; EXAMPLE:Check test for example
    ;; DESIGN-STRATEGY:Cases on collision of rectangle with boundary 
    (define (check-for-collision x speed)
      (cond
        [(< (+  x speed) HALF-SQUARE-WIDTH) HALF-SQUARE-WIDTH]
        [(> (+  x speed) RIGHT-BOUNDARY) RIGHT-BOUNDARY]
        [else (+ x speed)]))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; change-velocity:PosInt Int->Int
    ;; GIVEN:Speed with which the rectangle is moving and x-coordinate of rectangle.
    ;; RETURNS:The speed of rectangle reversed if it is hitting one of the walls.
    ;; EXAMPLE:Check test for example.
    ;; DESIGN-STRATERGY:Cases on collision of rectangle with boundary    
    (define (change-velocity speed x)
      (cond
        [(< (+ x speed) HALF-SQUARE-WIDTH) (- speed)]
        [(> (+ x speed) RIGHT-BOUNDARY)(- speed)]
        [else speed]))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-key-event : KeyEvent -> Square
    ;; Given:A key-event
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a Square ignores key events
    (define/public (after-key-event kev)
      this)      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-down : PosInt PosInt -> Square
    ;; GIVEN: the location of a button-down event
    ;; RETURNS:A Square object after button is down.
    ;; EXAMPLE:Check test for example.    
    ;; STRATEGY: Cases on whether the event is in the square
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
          (new Square%
               [x x][y y]
               [speed speed]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-up : PosInt PosInt -> Square
    ;; GIVEN: the location of a button-up event
    ;; RETURNS:Object after button is up    
    ;; STRATEGY: Combine Simpler Function
    ;; EXAMPLE:Check test for example    
    ;; If the Square is selected, then unselect it.
    (define/public (after-button-up mx my)      
      (new Square%
           [x x][y y]
           [speed speed]
           [selected? false]
           [saved-mx ZERO]
           [saved-my ZERO]))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-drag : PosInt PosInt -> Square
    ;; GIVEN: the location of a drag event
    ;; RETURNS:The square object after it has been dragged
    ;; EXAMPLE:Check test for example.    
    ;; STRATEGY: Cases on whether the square is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Square%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [speed speed]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN:A scene to place the square     
    ;; RETURNS: a scene like the given one, but with this square painted
    ;; on it.
    ;; EXAMPLE:Check test for example
    ;; DESIGN-STRATEGY:Combine simper functions.    
    (define/public (add-to-scene scene)
      (place-image SQUARE-IMG x y scene))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; in-square? : PosInt PosInt -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this square.
    ;; EXAMPLE:Check test for example
    ;; DESIGN-STRATEGY:Combine Simpler Functions.    
    (define (in-square?  cur-x cur-y)
      (and
       (<= 
        (- x (/ SQUARE-SIDE TWO))
        cur-x
        (+ x (/ SQUARE-SIDE TWO)))
       (<= 
        (- y (/ SQUARE-SIDE TWO))
        cur-y
        (+ y (/ SQUARE-SIDE TWO)))))
    
    
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-square-toy:PosInt PosInt PosInt->toy<%>
;; GIVEN:initial x and y coordinates of target and speed with which the square should move.
;; RETURNS:A Square object with square placed at given x and y coordinates and moving with
;; same speed.
;; EXAMPLE:Check test for example.
;; DESIGN-STRATEGY:Combine Simpler Functions.

(define (make-square-toy initial-x initial-y speed)
  (new Square% [x initial-x][y initial-y][speed speed]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Throbber%
  (class* object% (Toy<%>)
    ;; A Throbber contracts and expands
    ;; A Throbber is a(new Throbber% [x PosInt][y PosInt][count Integer][radius PosInt]
    ;;                               [selected? Boolean][saved-mx Integer][saved-my Integer])
    
    ;; the init-fields are the values that may vary from one Throbber to
    ;; the next.
    
    ; the x and y position of the center of the throbber and radius of throbber.
    (init-field x y count radius)   
    
    ; is the throbber selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the throbber is selected, the position of
    ;; the last button-down event inside the throbber, relative to the
    ;; throbbers center.  Else any value.
    (init-field [saved-mx ZERO] [saved-my ZERO])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; toy-x:->PosInt
    ;; RETURNS:The x coordinate of the toy.
    ;; Example:Check test for example.    
    ;; DESIGN-STRATERGY:Combine Simpler Functions    
    (define/public (toy-x)
      x)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; toy-y:->PosInt
    ;; RETURNS:The y coordinate of the toy.
    ;; Example:Check test for example.    
    ;; DESIGN-STRATERGY:Combine Simpler Functions   
    (define/public (toy-y)
      y)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; toy-data:->PosInt
    ;; RETURNS:The radius of the toy.
    ;; Example:Check test for example.    
    ;; DESIGN-STRATERGY:Combine Simpler Functions    
    (define/public (toy-data)
      radius)
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-tick : -> Throbber
    ;; RETURNS: A throbber like this one, but as it should be after a tick
    ;; a selected throbber doesn't move.
    ;; EXAMPLE:Check test at end of program for example  
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (new Throbber%
               [x x]
               [y y]
               [count (change-count count radius)]
               [radius (+ count radius)]
               [selected? selected?]
               [saved-mx saved-mx]
               [saved-my saved-my])))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; change-count:Int PosInt->Int
    ;; GIVEN:Radius of throbber and a counter
    ;; RETURNS:the change in counter value if the value of radius is equal to the max or min
    ;; radius value
    ;; EXAMPLE:Check test at end of program for example
    ;; STRATEGY:Use cases on value of radius.
    (define (change-count count radius)
      (if (or (< (+ count radius) THROBBER-MIN-RADIUS)
              (> (+ count radius) HALF-SQUARE-WIDTH))
          (- count)
          count))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-key-event : KeyEvent -> Throbber
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a throbber ignores key events
    (define/public (after-key-event kev)
      this)      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-button-down : PosInt PosInt -> Throbber
    ;; GIVEN: the location of a button-down event
    ;; RETURNS:the state of throbber after button is down
    ;; Example:Check test at end of program for example  
    ;; STRATEGY: Cases on whether the event is in the throbber.
    (define/public (after-button-down mx my)
      (if (in-throbber?  mx my)
          (new Throbber%
               [x x][y y]
               [count count]
               [radius radius]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-up : PosInt PosInt -> Throbber
    ;; GIVEN: the location of a button-up event
    ;; RETURNS:A object of thobber after button up event    
    ;; STRATEGY: Combine Simpler Functions
    ;; If the throbber is selected, then unselect it.
    (define/public (after-button-up mx my)
      (new Throbber%
           [x x][y y]
           [count count]
           [radius radius]
           [selected? false]
           [saved-mx saved-mx]
           [saved-my saved-my]))   
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-drag : PosInt PosInt -> Throbber
    ;; GIVEN: the location of a drag event
    ;; RETURN:The object throbber after it has been dragged    
    ;; STRATEGY: Cases on whether the throbber is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Throbber%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [count count]
               [radius radius]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN:A scene without throbber
    ;; RETURNS: a scene like the given one, but with this throbber painted
    ;; on it.
    ;; DESIGN-STRATEGY:Combine Simpler Functions    
    (define/public (add-to-scene scene)
      (place-image (circle radius "solid" GREEN) x y scene))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; in-throbber? : PosInt PosInt -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this throbber.
    ;; EXAMPLE:Check test for example
    ;; DESIGN-STRATEGY:Combine Simpler Functions.
    (define (in-throbber? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr radius)))
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-throbber:PosInt PosInt ->toy<%>
;; GIVEN:initial x and y coordinates of target where throbber should be placed.
;; RETURNS:A throbber object with throbber placed at given x and y coordinates.
;; EXAMPLE:Check test for example.
;; DESIGN-STRATEGY:Combine Simpler Functions.

(define (make-throbber initial-x initial-y)
  (new Throbber% [x initial-x][y initial-y][count TICK][radius THROBBER-MIN-RADIUS]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Clock%
  (class* object% (Toy<%>)
    ;; A Clock is a (new Clock% [x PosInt][y PosInt][count PosInt]
    ;;                        [selected? Boolean][saved-mx Integer][saved-my Integer]) 
    ;; the init-fields are the values that may vary from one Clock to
    ;; the next.
    ;; The clock counter will increment after every tick
    ; the x and y position of the center of the Clock and a count to keep count of number
    ;of ticks
    (init-field x y count)   
    
    ; is the clock selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the clock is selected, the position of
    ;; the last button-down event inside the clock, relative to the
    ;; clock's center.  Else any value.
    (init-field [saved-mx ZERO] [saved-my ZERO])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    ; the clock's side
    (field [s SQUARE-SIDE])   
    
    ; image for displaying the clock
    (field [CLOCK-IMG (square  s "outline" BLUE)])
    
    
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; toy-x:->PosInt
    ;; RETURNS:The x coordinate of the toy.
    ;; Example:Check test for example.    
    ;; DESIGN-STRATERGY:Combine Simpler Functions    
    (define/public (toy-x)
      x)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; toy-y:->PosInt
    ;; RETURNS:The y coordinate of the toy.
    ;; Example:Check test for example.    
    ;; DESIGN-STRATERGY:Combine Simpler Functions    
    
    (define/public (toy-y)
      y)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; toy-data:->PosInt
    ;; RETURNS:The clock tick of the toy.
    ;; Example:Check test for example.    
    ;; DESIGN-STRATERGY:Combine Simpler Functions    
    
    (define/public (toy-data)
      count)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-tick : -> Clock
    ;; RETURNS: A Clock like this one, but as it should be after a tick
    ;; a selected Clock doesn't move.
    ;; EXAMPLE:Check test at end of program for examples.    
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (new Clock%
               [x x]
               [y y]
               [count (+ TICK count)]
               [selected? selected?]
               [saved-mx saved-mx]
               [saved-my saved-my])))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-key-event : KeyEvent -> Clock
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a clock ignores key events
    (define/public (after-key-event kev)
      this)      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-down : PosInt PosInt -> Clock
    ;; GIVEN: the location of a button-down event
    ;; RETURNS:The Clock object after button down
    ;; EXAMPLE:Check test at end of program for examples.    
    ;; STRATEGY: Cases on whether the event is in the Clock
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
          (new Clock%
               [x x][y y]
               [count count]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-up : PosInt PosInt -> Clock
    ;; GIVEN: the location of a button-up event
    ;; RETURNS:The object clock after button up
    ;; EXAMPLE:Check test for example.    
    ;; STRATEGY: Combine Simpler Functions
    ;; If the Clock is selected, then unselect it.
    (define/public (after-button-up mx my)
      (new Clock%
           [x x][y y]
           [count count]
           [selected? false]
           [saved-mx saved-mx]
           [saved-my saved-my]))   
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-drag : PosInt PosInt -> Clock
    ;; GIVEN: the location of a drag event
    ;; RETURNS:The object of clock  while its getting dragged.
    ;; EXAMPLE:Check test for example.    
    ;; STRATEGY: Cases on whether the Clock is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Clock%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [count count]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN:A scene to place toy in it    
    ;; RETURNS: a scene like the given one, but with this clock painted
    ;; on it.
    ;; DESIGN-STRATEGY:Combine Simpler functions.    
    (define/public (add-to-scene scene)
      (place-image
       (overlay CLOCK-IMG (text (number->string count) TWELVE BLACK))  x y scene))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; in-square? : PosInt PosInt -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this Clock.
    ;; EXAMPLE:Check test for example.    
    ;; DESIGN-STRATEGY:Combine Simpler Functions.    
    (define (in-square?  cur-x cur-y)
      (and
       (<= 
        (- x HALF-SQUARE-WIDTH)
        cur-x
        (+ x HALF-SQUARE-WIDTH))
       (<= 
        (- y HALF-SQUARE-WIDTH)
        cur-y
        (+ y HALF-SQUARE-WIDTH))))
    
    
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-clock:PosInt PosInt ->toy<%>
;; GIVEN:initial x and y coordinates of target.
;; RETURNS:A Clock object with Clock placed at given x and y coordinates .
;; EXAMPLE:Check test for example.
;; DESIGN-STRATEGY:Combine Simpler Functions.

(define (make-clock initial-x initial-y)
  (new Clock% [x initial-x][y initial-y][count ZERO]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Football%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from Football to
    ;; the next.
    ;; The football image contacts in size after every tick
    ;; A Football is a (new Football% [x PosInt][y PosInt][img Image]
    ;;                    [selected? Boolean][saved-mx Integer][saved-my Integer])
    ;; these 3 are optional
    ; the x and y position of the center of the Football and img to keep the instance of
    ;image.
    (init-field x y img)   
    
    ; is the Football selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the Football is selected, the position of
    ;; the last button-down event inside the Football, relative to the
    ;; Football center.  Else any value.
    (init-field [saved-mx ZERO] [saved-my ZERO])
    (init-field [factor TICK])
    
    
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; toy-x:->PosInt
    ;; RETURNS:The x coordinate of the toy.
    ;; Example:Check test for example.    
    ;; DESIGN-STRATERGY:Combine Simpler Functions    
    (define/public (toy-x)
      x)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; toy-y:->PosInt
    ;; RETURNS:The y coordinate of the toy.
    ;; Example:Check test for example.    
    ;; DESIGN-STRATERGY:Combine Simpler Functions    
    (define/public (toy-y)
      y)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; toy-data:->PosInt
    ;; RETURNS:The area of the toy.
    ;; Example:Check test for example.    
    ;; DESIGN-STRATERGY:Combine Simpler Functions    
    
    (define/public (toy-data)
      (* (image-width  img)(image-height img)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-tick :  -> Football   
    ;; RETURNS: A Football like this one, but as it should be after a tick
    ;; a selected Football doesn't move.
    ;; Example:Check test at end of program for example.    
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (new Football%
               [x x]
               [y y]
               [img (scale factor img)]
               [factor (* factor 0.7)]
               [selected? selected?]
               [saved-mx saved-mx]
               [saved-my saved-my])))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-key-event : KeyEvent -> Football
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a Football ignores key events
    (define/public (after-key-event kev)
      this)      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-down : PosInt PosInt -> Football
    ;; GIVEN: the location of a button-down event
    ;; RETURNS:An object of Football class when button is down
    ;; Example:Check test for example.    
    ;; STRATEGY: Cases on whether the event is in the Football
    (define/public (after-button-down mx my)
      (if (in-image? mx my)
          (new Football%
               [x x][y y]
               [img img]
               [factor factor]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-up : PosInt PosInt -> Football
    ;; GIVEN: the location of a button-up event
    ;; RETURN:An object of class Football after button is up    
    ;; STRATEGY: Combine Simpler Functions
    ;; If the Football is selected, then unselect it.
    (define/public (after-button-up mx my)
      (new Football%
           [x x][y y]
           [img img]
           [factor factor]
           [selected? false]
           [saved-mx saved-mx]
           [saved-my saved-my]))   
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-drag : PosInt PosInt -> Football
    ;; GIVEN: the location of a drag event
    ;; RETURN:An object of class Football after-drag    
    ;; STRATEGY: Cases on whether the Football is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Football%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [img img]
               [factor factor]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN:A scene to place toy.    
    ;; RETURNS: a scene like the given one, but with this football painted
    ;; on it.
    ;; STRATEGY:Combine Simpler functions.    
    (define/public (add-to-scene scene)
      (if
       (>= factor 0.3)
       (place-image img x y scene)scene))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; in-image? : PosInt PosInt -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this image.
    ;; EXAMPLE:Check test for example.    
    ;; Design-Strategy:Combine Simpler Functions.
    
    (define (in-image?  cur-x cur-y)
      (and
       (<= 
        (- x HALF-FOOT-WIDTH)
        cur-x
        (+ x HALF-FOOT-WIDTH))
       (<= 
        (- y HALF-FOOT-HEIGHT)
        cur-y
        (+ y HALF-FOOT-HEIGHT))))
    
    
    ))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Target%
  (class* object% (Widget<%>)
    
    ;; the init-fields are the values that may vary from one target to
    ;; the next.
    ;; Is used to create the target object
    ;; A Target is a (new Target% [x PosInt][y PosInt]
    ;;                        [selected? Boolean][saved-mx Integer][saved-my Integer])
    ; the x and y position of the center of the target
    (init-field x y)   
    
    ; is the target selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the target is selected, the position of
    ;; the last button-down event inside the target, relative to the
    ;; target's center.  Else any value.
    (init-field [saved-mx ZERO] [saved-my ZERO])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    ; the target's radius
    (field [r TARGET-RADIUS])
    (field [TAR-IMG (circle r "outline" BLACK)])  
    
    ; image for displaying the target
    
    
    
    
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-tick : -> Target
    ;; RETURNS: A target like this one, but as it should be after a tick
    ;; a selected target doesn't move.
    ;; STRATEGY: Combine Simpler Functions. 
    (define/public (after-tick) this)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-key-event : KeyEvent -> Target
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a Target ignores key events
    ;; Strategy:Combine Simpler Functions    
    (define/public (after-key-event kev) this)      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-button-down : PosInt PosInt -> Target
    ;; GIVEN: the location of a button-down event
    ;; RETURNS:A target after-button-down.    
    ;; STRATEGY: Cases on whether the event is in the target
    (define/public (after-button-down mx my)
      (if (in-target? mx my)
          (new Target%
               [x x][y y]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-up : PosInt PosInt -> Target
    ;; GIVEN: the location of a button-up event
    ;; RETURNS:A target after button is up.    
    ;; STRATEGY: Combine Simpler Functions.
    ;; If the Target is selected, then unselect it.
    (define/public (after-button-up mx my)
      (new Target%
           [x x][y y]
           [selected? false]
           [saved-mx saved-mx]
           [saved-my saved-my]))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-drag : PosInt PosInt -> Target
    ;; GIVEN: two PosInt showing the location of a drag event
    ;; RETURNS:A object of target.    
    ;; STRATEGY: case on whether the target is selected
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Target%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene as background
    ;; RETURNS: a scene like the given one, but with this target painted
    ;; on it.
    ;; Strategy:Combine Simpler functions
    ;; Example:Check test for example.    
    (define/public (add-to-scene scene)
      (place-image TAR-IMG x y scene))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; in-target? : PosInt PosInt -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this target.
    ;; Example:Check test at end for example.
    ;; Design-Stratergy: Combine Simpler Functions.   
    (define (in-target? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-football:PosInt PosInt ->toy<%>
;; GIVEN:initial x and y coordinates of target.
;; RETURNS:A Football object with Football placed at given x and y coordinates .
;; EXAMPLE:Check test for example.
(define (make-football initial-x initial-y)
  (new Football% [x initial-x][y initial-y][img FOOTBALL]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-world: PosInt : -> WorldState
;; GIVEN: a PosInt as travel speed of rectangles
;; RETURNS: a world with the target set in the middle of the canvas
;; DESIGN-STRATEGY:Generalization over make-world-state
(define (make-world rectanglespeed)
  (make-Playground
   empty
   (new Target%
        [x (/ CANVAS-WIDTH TWO)]
        [y (/ CANVAS-HEIGHT TWO)]
        [selected? false]
        [saved-mx ZERO]
        [saved-my ZERO])
   rectanglespeed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; run : PosNum PosInt -> World
; GIVEN: a frame rate, in secs/tick and an Pos showing the travel speed of rectangle
; EFFECT: runs an initial world at the given frame rate
; RETURNS: the final state of the world
; STRATEGY: generalization over big-bang
(define (run rate rectanglespeed)
  (big-bang (make-world rectanglespeed)
            
            
            (on-tick
             (lambda (w) (send w after-tick))
             rate)
            (on-draw
             (lambda (w) (send w to-scene)))
            (on-key
             (lambda (w kev)
               (send w after-key-event kev)))
            (on-mouse
             (lambda (w mx my mev)
               (send w after-mouse-event mx my mev)))))
(begin-for-test
  (check-equal? (send (make-world 1) target-selected?) #f)
  (check-equal? (send (make-world 1) target-x) (/ CANVAS-WIDTH 2))
  (check-equal? (send (make-world 1) target-y) (/ CANVAS-HEIGHT 2))
  
  ; Throbber
  (check-equal?
   (send 
    (send 
     (new Throbber%
          [x 250][y 300][selected? #t][saved-mx 0][saved-my 0][count 0][radius 5])
     after-tick) toy-x)250 "should return same throbber")
  (check-equal?
   (send 
    (send 
     (new Throbber%
          [x 250][y 300][selected? #t][saved-mx 0][saved-my 0][count 0][radius 5])
     after-key-event "w") toy-x)250 "should return same throbber")
  (check-equal?
   (dynamic-get-field 'count 
                      (send 
                       (new Throbber%
                            [x 250][y 300][selected? #f][saved-mx 0]
                            [saved-my 0][count 5][radius 25])
                       after-tick)) -5 "should start to shrink")
  
  (check-equal?
   (send
    (first
     (send
      (send (make-world 1) after-key-event "t") get-toys)) add-to-scene EMPTY-CANVAS)
   (place-image (circle 5 "solid" "green") 250 300 EMPTY-CANVAS)
   "render world"
   )
  (check-equal?
   (send (first (send (send (make-world 1) after-key-event "t") get-toys)) toy-x)
   (/ CANVAS-WIDTH 2) "initial position is in the center of canvas")
  (check-equal?
   (send (first (send (send (make-world 1) after-key-event "t") get-toys)) toy-y)
   (/ CANVAS-HEIGHT 2) "initial position is in the center of canvas")
  (check-equal?
   (send (first (send (send (make-world 1) after-key-event "t") get-toys)) toy-data)
   5 "should return initial radius")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "t") get-toys))
                                       after-button-down 250 300))
   
   #t "Should be selected")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "t") get-toys))
                                       after-button-down 600 300))
   
   #f "should not be selected")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "t") get-toys))
                                       after-button-up 600 300))
   
   #f "should be released")
  (check-equal?
   (dynamic-get-field 'x (send (first (send (send (make-world 1)
                                                  after-key-event "t") get-toys))
                               after-drag 600 300))
   
   250 "should return x position")
  (check-equal?
   (dynamic-get-field 'y (send (first (send (send (make-world 1)
                                                  after-key-event "t") get-toys))
                               after-drag 600 300))
   
   300 "should return y position")
  (check-equal?
   (dynamic-get-field 'count (send (first (send (send (make-world 1)
                                                      after-key-event "t") get-toys))
                                   after-tick))1 "should return count 1")
  (check-equal?
   (send (send (send (first (send (send (make-world 1) after-key-event "t") get-toys))
                     after-button-down 250 300) after-drag 400 500) toy-x)
   400
   "should move along mouse")
  (check-equal?
   (send (send (send (first (send (send (make-world 1) after-key-event "t") get-toys))
                     after-button-down 250 300) after-drag 400 500) toy-y)
   500
   "should move along mouse")
  
  ;; Square
  (check-equal?
   (send 
    (send
     (new Square% [x 200][y 100][speed 20][selected? #t][saved-mx 0][saved-my 0])
     after-tick) toy-x)
   200
   "should not move")
  (check-equal?
   (send 
    (send
     (new Square% [x 479][y 100][speed 20][selected? #f][saved-mx 0][saved-my 0])
     after-tick) toy-x)
   480
   "should bounce")
  (check-equal?
   (send 
    (send
     (new Square% [x 30][y 100][speed -20][selected? #f][saved-mx 0][saved-my 0])
     after-tick) toy-x)
   20
   "should bounce")
  (check-equal?
   (send
    (first
     (send
      (send (make-world 1) after-key-event "s") get-toys)) add-to-scene EMPTY-CANVAS)
   (place-image (square SQUARE-SIDE "outline" "blue") 250 300 EMPTY-CANVAS)
   "render world")
  (check-equal?
   (send (first (send (send (make-world 1) after-key-event "s") get-toys)) toy-x)
   (/ CANVAS-WIDTH 2) "initial position is in the center of canvas")
  (check-equal?
   (send (first (send (send (make-world 1) after-key-event "s") get-toys)) toy-y)
   (/ CANVAS-HEIGHT 2) "initial position is in the center of canvas")
  (check-equal?
   (send (first (send (send (make-world 2) after-key-event "s") get-toys)) toy-data)
   2 "should return initial speed")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "s") get-toys))
                                       after-button-down 250 300))
   
   #t "Should be selected")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "s") get-toys))
                                       after-button-down 600 300))
   
   #f "should not be selected")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "s") get-toys))
                                       after-button-up 600 300))
   
   #f "should be released")
  (check-equal?
   (dynamic-get-field 'x (send (first (send (send (make-world 1) after-key-event "s")
                                            get-toys))
                               after-drag 600 300))
   
   250 "should return x position")
  (check-equal?
   (dynamic-get-field 'y (send (first (send (send (make-world 1) after-key-event "s")
                                            get-toys))
                               after-drag 600 300))
   
   300 "should return y position")
  (check-equal?
   (dynamic-get-field 's (send (first (send (send (make-world 1) after-key-event "s")
                                            get-toys))
                               after-tick))SQUARE-SIDE "should return SQUARE-SIDE")
  (check-equal?
   (send (send (send (first (send (send (make-world 1) after-key-event "s") get-toys))
                     after-button-down 250 300) after-drag 400 500) toy-x)
   400
   "should move along mouse")
  (check-equal?
   (send (send (send (first (send (send (make-world 1) after-key-event "s") get-toys))
                     after-button-down 250 300) after-drag 400 500) toy-y)
   500
   "should move along mouse")
  ;; Football
  (check-equal?
   (send
    (first
     (send
      (send (make-world 1) after-key-event "f") get-toys)) add-to-scene EMPTY-CANVAS)
   (place-image FOOTBALL 250 300 EMPTY-CANVAS)
   "should render world"
   )
  (check-equal?
   (send
    (send 
     (new Football%
          [x 250][y 300][img FOOTBALL][selected? #t][saved-mx 0][saved-my 0][factor 0.1])
     after-tick) toy-x) 250 "should stay the same")
  (check-equal?
   (send
    (send 
     (new Football%
          [x 250][y 300][img FOOTBALL][selected? #f][saved-mx 0][saved-my 0][factor 0.1])
     after-key-event "w") toy-x) 250 "should stay the same")
  (check-equal?
   (send 
    (new Football%
         [x 250][y 300][img FOOTBALL][selected? #f][saved-mx 0][saved-my 0][factor 0.1])
    add-to-scene EMPTY-CANVAS) EMPTY-CANVAS "should render empty world")
  
  (check-equal?
   (send (first (send (send (make-world 1) after-key-event "f") get-toys)) toy-x)
   (/ CANVAS-WIDTH 2) "initial position is in the center of canvas")
  (check-equal?
   (send (first (send (send (make-world 1) after-key-event "f") get-toys)) toy-y)
   (/ CANVAS-HEIGHT 2) "initial position is in the center of canvas")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "f") get-toys))
                                       after-button-down 250 300))
   
   #t "Should be selected")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "f") get-toys))
                                       after-button-down 600 300))
   
   #f "should not be selected")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "f") get-toys))
                                       after-button-up 600 300))
   
   #f "should be released")
  (check-equal?
   (dynamic-get-field 'x (send (first (send (send (make-world 1)
                                                  after-key-event "f") get-toys))
                               after-drag 600 300))
   
   250 "should return x position")
  (check-equal?
   (dynamic-get-field 'y (send (first (send (send (make-world 1)
                                                  after-key-event "f") get-toys))
                               after-drag 600 300))
   
   300 "should return y position")
  (check-equal?
   (dynamic-get-field 'factor (send (first (send (send (make-world 1)
                                                       after-key-event "f") get-toys))
                                    after-tick))0.7 "should return initial factor")
  (check-equal?
   (send
    (send
     (send
      (first (send (send (send (make-world 1) after-key-event "f")
                         after-mouse-event 400 500 "drag")
                   get-toys))
      after-button-down 250 300) after-drag 400 500) toy-x)
   400 "should return the x position to 400 after selection and drag")
  (check-equal?
   (send
    (send
     (send
      (first (send (send (send (make-world 1) after-key-event "f")
                         after-mouse-event 400 500 "drag")
                   get-toys))
      after-button-down 250 300) after-drag 400 500) toy-y)
   500 "should return the y position to 500 after selection and drag")
  (check-equal?
   (send (first (send (send (make-world 2) after-key-event "f") get-toys)) toy-data)
   13689 "the image size is 13689")
  ;;Clock
  (check-equal?
   (send
    (send 
     (new Clock%
          [x 250][y 300][count 0][selected? #t][saved-mx 0][saved-my 0])
     after-tick) toy-x) 250 "should stay the same")
  (check-equal?
   (send
    (send 
     (new Clock%
          [x 250][y 300][count 0][selected? #f][saved-mx 0][saved-my 0])
     after-key-event "w") toy-x) 250 "should stay the same")
  (check-equal?
   (send
    (first
     (send
      (send (make-world 1) after-key-event "w") get-toys)) add-to-scene EMPTY-CANVAS)
   (place-image
    (overlay (square SQUARE-SIDE "outline" "blue")
             (text (number->string 0) 12 "black")) 250 300 EMPTY-CANVAS)
   "should render world"
   )
  
  (check-equal?
   (send (first (send (send (make-world 1) after-key-event "w") get-toys)) toy-x)
   (/ CANVAS-WIDTH 2) "initial position is in the center of canvas")
  (check-equal?
   (send (first (send (send (make-world 1) after-key-event "w") get-toys)) toy-y)
   (/ CANVAS-HEIGHT 2) "initial position is in the center of canvas")
  (check-equal?
   (send (first (send (send (make-world 2) after-key-event "w") get-toys)) toy-data)
   0 "should return initial count")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "w") get-toys))
                                       after-button-down 250 300))
   
   #t "Should be selected")
  (check-equal?
   (dynamic-get-field 'selected? (send (first (send (send (make-world 1)
                                                          after-key-event "w") get-toys))
                                       after-button-down 600 300))
   
   #f "should not be selected")
  (check-equal?
   (dynamic-get-field 'selected? (first
                                  (send
                                   (send 
                                    (send (make-world 1) after-key-event "w")
                                    after-mouse-event 600 300 "button-up")
                                   get-toys)))
   #f "should be released")
  (check-equal?
   (dynamic-get-field 'selected? (first
                                  (send
                                   (send 
                                    (send
                                     (make-world 1)
                                     after-key-event "w")
                                    after-mouse-event 600 300 "button-down")
                                   get-toys)))
   #f "should not be selected")
  (check-equal?
   (dynamic-get-field 'x (send
                          (first
                           (send (send (make-world 1) after-key-event "w") get-toys))
                               after-drag 600 300))
   
   250 "should return x position")
  (check-equal?
   (dynamic-get-field 'y (send
                          (first
                           (send (send (make-world 1) after-key-event "w") get-toys))
                               after-drag 600 300))
   
   300 "should return y position")
  (check-equal?
   (dynamic-get-field 's (send
                          (first
                           (send (send (make-world 1) after-key-event "w") get-toys))
                               after-tick))SQUARE-SIDE "should return SQUARE-SIDE")
  (check-equal?
   (dynamic-get-field 'count
                      (first (send
                              (send (send (make-world 1) after-key-event "w") after-tick)
                              get-toys))) 1 "should return SQUARE-SIDE")
  
  (check-equal?
   (send (send (send (first (send (send (make-world 1) after-key-event "w") get-toys))
                     after-button-down 250 300) after-drag 400 500) toy-x)
   400 "the x position after drag should be 400")
  (check-equal?
   (send (send (send (first (send (send (make-world 1) after-key-event "w") get-toys))
                     after-button-down 250 300) after-drag 400 500) toy-y)
   500 "the y position after drag should be 500")
  ;;Target
  (check-equal?
   (dynamic-get-field 'selected?
                      (send (dynamic-get-field 'target (send (make-world 1)
                                                             after-key-event "w"))
                            after-button-down 250 300))
   #t "should selectet target")
  (check-equal?
   (dynamic-get-field 'x
                      (send (dynamic-get-field 'target (send (make-world 1)
                                                             after-key-event "w"))
                            after-drag 350 400))
   250
   "should not move when not selected")
  (check-equal?
   (dynamic-get-field 'y
                      (send (dynamic-get-field 'target (send (make-world 1)
                                                             after-key-event "w"))
                            after-drag 350 400))
   300 "should not move when not selected")
  (check-equal?
   (dynamic-get-field 'x
                      (send
                       (send
                        (dynamic-get-field 'target (send (make-world 1)
                                                         after-key-event "w"))
                        after-button-down 250 300)
                       after-drag 350 400))
   350 "should move along mouse")
  (check-equal?
   (dynamic-get-field 'y
                      (send
                       (send
                        (dynamic-get-field 'target (send (make-world 1)
                                                         after-key-event "w"))
                        after-button-down 250 300)
                       after-drag 350 400))
   400 "should move along mouse")
  ;; general
  (check-equal?
   (send (send (make-world 1)  after-key-event "s") to-scene) 
   (place-image (square SQUARE-SIDE "outline" "blue") 250 300
                (place-image (circle TARGET-RADIUS "outline" "black") 250 300
                             EMPTY-CANVAS))
   "should render world")
  (check-equal?
   (length
    (send
     (send
      (send
       (make-world 1)  after-key-event "s") after-key-event "v") get-toys))
   1 "should still have same objects"
   )
  (check-equal?
   (length (send (send (send (make-world 1)  after-key-event "s")
                       after-mouse-event 250 300 "move") get-toys))
   1 "should still have same objects"
   )
  (check-equal?
   (dynamic-get-field 'x 
                      (send 
                       (new Target% [x 250][y 300][selected? #t][saved-mx 0][saved-my 0])
                       after-tick))
   250 "should not act to after-tick")
  (check-equal?
   (dynamic-get-field 'x 
                      (send 
                       (new Target% [x 250][y 300][selected? #t][saved-mx 0][saved-my 0])
                       after-key-event "w"))
   250 "should not act to after-key-event"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
