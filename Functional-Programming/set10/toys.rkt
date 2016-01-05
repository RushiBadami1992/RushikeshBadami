#lang racket
;; Implement Problem set 9 toy.rkt with using WidgetWorks framework

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interface.rkt")
(provide 
         make-Playground
         make-square-toy
         make-throbber
         make-clock
         run
         make-football
         Toy<%>
         PlaygroundState<%>)
(check-location "10" "toys.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT ))
(define SQUARE-SIDE 40)
(define THROBBER-MIN-RADIUS 5)
(define THROBBER-MAX-RADIUS 20)
(define HALF-SQUARE-WIDTH 20)
(define RIGHT-BOUNDARY (- CANVAS-WIDTH HALF-SQUARE-WIDTH))
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
(define current-world (make-world CANVAS-WIDTH CANVAS-HEIGHT))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DESIGN

#|
a ListOfToy<%>(LOT) is a list which is either:
-- empty
-- (cons Toy<%> LOT)

lot-fn: LOT -> ??
(define (lot-fn lot)
    (cond
        [(empty?) ...]
        [else ... (first lot)
              ... (lot-fn (rest lot))]))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Playground% class
;; A Playground is a(new Playground% [objs LOT][speed Integer][world World])
;; Playground consist of toys
(define Playground%
  (class* object% (PlaygroundState<%>)
;;This field is a list of toy objects
(init-field objs) ;  ListOfWidget
;; These init-field is you to initalize target and speed with which rectangle should
;; travel 
(init-field speed)
(init-field  world)
(super-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target-x: ->PosInt
;; Returns:The x coordinate of target
;; Example:Check test at end of program for example.
;; Design-Strategy:Combine Simpler Functions.
    
    (define/public (target-x)
     (dynamic-get-field 'x (first (reverse objs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target-y: ->PosInt
;; Returns:The y coordinate of target
;; Example:Check test at end of program for example.
;; Design-Strategy:Combine Simpler Functions.    
  (define/public (target-y)
      (dynamic-get-field 'y (first(reverse objs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; target-selected?: ->Boolean
;; Returns:True iff target is selected.
;; Example:Check test at end of program for example.
;; Design-Strategy:Combine Simpler Functions.    
    
  (define/public (target-selected?)
      (dynamic-get-field 'selected? (first (reverse objs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-tick: ->Playground
;; Returns:A playground object.
;; Example:Check test at end of program for example.
;; Design-Stratergy:Use HOF Map on LOT.    
  (define/public (after-tick)
     (make-Playground
       (map
        (lambda (obj) (send obj after-tick))
        objs)))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;; add-to-scene : Scene -> Scene
;; RETURNS:A scene with all toys placed on it
;; Example:Check test at end of program for example.   
;; DESIGN-STRATERGY:Use HOF foldr on the Widget's in this World
  (define/public (add-to-scene scene)
     (foldr
      (lambda (obj scene)
         (send obj add-to-scene scene))
      EMPTY-CANVAS
      objs))
     
      
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-key-event : Keyevent -> void
;; GIVEN: A keyevent   
;; Effect:Adds a toy to the ListOfToy.
;; EXAMPLE:Check test at end of problem for example.    
;; STRATEGY: Cases on kev

    (define/public (after-key-event kev)
      (cond
        [(key=? kev SQUARE)
         (set!  objs
          (cons (make-square-toy (target-x) (target-y) speed) objs))]
         [(key=? kev THROBBER)
          (set!  objs
          (cons (make-throbber (target-x) (target-y)) objs))]
         [(key=? kev CLOCK)
         (set!  objs
          (cons (make-clock (target-x) (target-y)) objs))]
         [(key=? kev BALL)
        (set!  objs
          (cons (make-football (target-x) (target-y)) objs))]
        [else
         (map
           (lambda (obj) (send obj after-key-event kev))
           objs)]))
         
;; the next few functions are local functions, not in the interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-down:PosInt PosInt -> Playground
;; GIVEN:X and Y coordinates of current position of mouse.
;; RETURNS:The playground after mouse down event.
;; EXAMPLE:Check test for example.
;; STRATEGY:Use HOF map on LOT    
  (define/public (after-button-down cur-mx cur-my)
     (make-Playground
       (map
        (lambda (obj) (send obj after-button-down cur-mx cur-my))
        objs)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; after-button-up:PosInt PosInt -> Playground
;; GIVEN:X and Y coordinates of current position of mouse.
;; RETURNS:The playground after mouse up event.
;; EXAMPLE:Check test for example.
;; STRATEGY:Use HOF map on LOT    
  (define/public (after-button-up cur-mx cur-my)
      (make-Playground
      (map
        (lambda (obj) (send obj after-button-up cur-mx cur-my))
        objs)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-drag:PosInt PosInt -> Playground
;; GIVEN:X and Y coordinates of current position of mouse.
;; RETURNS:The playground when image is getting dragged.
;; EXAMPLE:Check test for example.
;; STRATEGY:Use HOF map on LOT    
  (define/public (after-drag current-mx current-my)
      (make-Playground
       (map
        (lambda (obj) (send obj after-drag current-mx current-my))
        objs)))
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
;; after-tick : -> void
;; Effect: A square  with its x-coordinate and speed changed, but as it should be
;;    after a tick a selected square doesn't move.
;; EXAMPLE:Check Test for example.
;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (let ((x1 (check-for-collision))
                (speed1 (change-velocity)))
            (begin
              (set! speed speed1)
              (set! x x1)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-for-collision: -> PosInt
;; GIVEN:Speed with which the rectangle is moving and x-coordinate of rectangle.
;; RETURNS:The X coordinate of the rectangle for current clock tick
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Cases on collision of rectangle with boundary 
 (define (check-for-collision)
      (cond
        [(< (+  x speed) HALF-SQUARE-WIDTH) HALF-SQUARE-WIDTH]
        [(> (+  x speed) RIGHT-BOUNDARY) RIGHT-BOUNDARY]
        [else (+ x speed)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change-velocity: ->Int
;; GIVEN:Speed with which the rectangle is moving and x-coordinate of rectangle.
;; RETURNS:The speed of rectangle reversed if it is hitting one of the walls.
;; EXAMPLE:Check test for example.
;; DESIGN-STRATERGY:Cases on collision of rectangle with boundary    
  (define (change-velocity)
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
;; after-button-down : PosInt PosInt -> void
;; GIVEN: the location of a button-down event
;; Effect:An object with its saved-mx and saved-my values updated after button is down.
;; EXAMPLE:Check test for example.    
;; STRATEGY: Cases on whether the event is in the square
 (define/public (after-button-down mx my)
      (if (in-square? mx my)
          (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
           this))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-up : PosInt PosInt -> void
;; GIVEN: the location of a button-up event
;; Effect:The selected field updated after button is up    
;; STRATEGY: Combine Simpler Function
;; EXAMPLE:Check test for example
;; If the Square is selected, then unselect it.
 (define/public (after-button-up mx my)      
      (set! selected? false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; after-drag : Int Int -> void
;; GIVEN: the location of a drag event
;; Effect:The updated values of mouse coordinates of object after it has been dragged
;; EXAMPLE:Check test for example.    
;; STRATEGY: Cases on whether the square is selected.
;; If it is selected, move it so that the vector from the center to
;; the drag event is equal to (mx, my)
  (define/public (after-drag mx my)
      (if selected?
          (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
          this))   
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-to-scene : Scene -> Scene
;; GIVEN:A scene to place the square     
;; RETURNS: a scene like the given one, but with this square painted
;; on it.
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Combine Simper functions.    
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

 (define/public (for-test:x)          x)
 (define/public (for-test:selected?)  selected?)
   
 ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-square-toy:PosInt PosInt PosInt->Toy<%>
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
    ;;                            [selected? Boolean][saved-mx Integer][saved-my Integer])
    
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
;; DESIGN-STRATEGY:Combine Simpler Functions    
  (define/public (toy-x)
      x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toy-y:->PosInt
;; RETURNS:The y coordinate of the toy.
;; Example:Check test for example.    
;; DESIGN-STRATEGY:Combine Simpler Functions   
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
;; after-tick : -> void
;; Effect: The value of throbber updated after a tick
;; a selected throbber doesn't move.
;; EXAMPLE:Check test at end of program for example  
;; STRATEGY: Cases on selected?
 (define/public (after-tick)
      (if selected?
          this
          (let ((count1 (change-count)))
              (begin
            (set! count count1)
            (set! radius (+ radius count))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change-count: -> Int
;; GIVEN:Radius of throbber and a counter
;; Effect:the change in counter value if the value of radius is equal to the max or min
;; radius value
;; EXAMPLE:Check test at end of program for example
;; STRATEGY:Use cases on value of radius.
 (define (change-count)
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
;; after-button-down : PosInt PosInt -> void
;; GIVEN: the location of a button-down event
;; Effect:the values of throbber updated  after button is down
;; Example:Check test at end of program for example  
;; STRATEGY: Cases on whether the event is in the throbber.
 (define/public (after-button-down mx my)
      (if (in-throbber?  mx my)
          (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
          this))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-up : PosInt PosInt -> void
;; GIVEN: the location of a button-up event
;; Effect:A values of  of thobber updated after button up event    
;; STRATEGY: Combine Simpler Functions
;; If the throbber is selected, then unselect it.
 (define/public (after-button-up mx my)
      (set! selected? false))   
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-drag : Int Int -> void
;; GIVEN: the location of a drag event
;; Effect:The value of  throbber updated  after it has been dragged    
;; STRATEGY: Cases on whether the throbber is selected.
;; If it is selected, move it so that the vector from the center to
;; the drag event is equal to (mx, my)
 (define/public (after-drag mx my)
      (if selected?
          (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
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
;;These functions are used for testing    
   (define/public (for-test:count) count)
    (define/public (for-test:selected?) selected?)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-throbber:PosInt PosInt ->Toy<%>
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
;; after-tick : -> void
;; Effect: A Clock like this one, but with its count updated  after a tick
;; a selected Clock doesn't move.
;; EXAMPLE:Check test at end of program for examples.    
;; STRATEGY: Cases on selected?
  (define/public (after-tick)
      (if selected?
          this
         (set! count (add1 count))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; after-key-event : KeyEvent -> Clock
;; RETURNS: A world like this one, but as it should be after the
;; given key event.
;; DETAILS: a clock ignores key events
 (define/public (after-key-event kev)
      this)      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-down : PosInt PosInt -> void
;; GIVEN: the location of a button-down event
;; Effect:A clock with its selected? and mouse co ordinates updated.
;; EXAMPLE:Check test at end of program for examples.    
;; STRATEGY: Cases on whether the event is in the Clock
(define/public (after-button-down mx my)
      (if (in-square? mx my)
          (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
          this))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-up : PosInt PosInt -> void
;; GIVEN: the location of a button-up event
;; Effect:The object of clock updated after button up
;; EXAMPLE:Check test for example.    
;; STRATEGY: Combine Simpler Functions
;; If the Clock is selected, then unselect it.
(define/public (after-button-up mx my)
    (set! selected? false))   
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-drag : Int Int -> void
;; GIVEN: the location of a drag event
;; Effect:The object of clock updated while its getting dragged.
;; EXAMPLE:Check test for example.    
;; STRATEGY: Cases on whether the Clock is selected.
;; If it is selected, move it so that the vector from the center to
;; the drag event is equal to (mx, my)
(define/public (after-drag mx my)
   (if selected?
    (begin
    (set! x (- mx saved-mx))
    (set! y (- my saved-my)))
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
    
    ;;This function is used for testing.
    (define/public (for-test:selected?) selected?)

    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-clock:PosInt PosInt ->Toy<%>
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
    ;;                    [selected? Boolean][saved-mx Integer][saved-my Integer]
    ;;                    [factor PosReal])
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
;; after-tick :  -> void   
;; Effect: A Football like this one, but as it should be after a tick
;; a selected Football doesn't move.
;; Example:Check test at end of program for example.    
;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (begin
          (set! img (scale (* 0.7 factor) img))
          (set! factor (* factor 0.7)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; after-key-event : KeyEvent -> Football
;; RETURNS: A world like this one, but as it should be after the
;; given key event.
;; DETAILS: a Football ignores key events
  (define/public (after-key-event kev)
      this)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-down : PosInt PosInt -> void
;; GIVEN: the location of a button-down event
;; Effect:An object of Football updated after button is down
;; Example:Check test for example.    
;; STRATEGY: Cases on whether the event is in the Football
  (define/public (after-button-down mx my)
      (if (in-image? mx my)
          (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
          this))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-up : PosInt PosInt -> void
;; GIVEN: the location of a button-up event
;; Effect:An object of class Football updated after button is up else returns void.    
;; STRATEGY: Combine Simpler Functions
;; If the Football is selected, then unselect it.
  (define/public (after-button-up mx my)
   (set! selected? false))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-drag : Int Int -> void
;; GIVEN: the location of a drag event
;; Effect:An object of class Football updated  afer  drag else returns void.    
;; STRATEGY: Cases on whether the Football is selected.
;; If it is selected, move it so that the vector from the center to
;; the drag event is equal to (mx, my)
  (define/public (after-drag mx my)
      (if selected?
          (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
          this))   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-to-scene : Scene -> Scene
;; GIVEN:A scene to place toy.    
;; RETURNS: a scene like the given one, but with this football painted
;; on it.
;; STRATEGY:Combine Simpler functions.    
  (define/public (add-to-scene scene)
      (if
       (>= factor 0.1)
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
    
     (define/public (for-test:selected?) selected?)
    ))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Target%
  (class* object% (SWidget<%>)
    
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
;; after-button-down : PosInt PosInt -> void
;; GIVEN: the location of a button-down event
;; Effect:A target with its value updated after-button-down.    
;; STRATEGY: Cases on whether the event is in the target
 (define/public (after-button-down mx my)
      (if (in-target? mx my)
          (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
           this))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-up : PosInt PosInt -> void
;; GIVEN: the location of a button-up event
;; Effect:A target with its values updated after button is up.    
;; STRATEGY: Combine Simpler Functions.
;; If the Target is selected, then unselect it.
  (define/public (after-button-up mx my)
      (set! selected? false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-drag : PosInt PosInt -> void
;; GIVEN: two PosInt showing the location of a drag event
;; Effect:An object of target with its value updated when mouse is dragged.    
;; STRATEGY: case on whether the target is selected
;; If it is selected, move it so that the vector from the center to
;; the drag event is equal to (mx, my)
  (define/public (after-drag mx my)
      (if selected?
          (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
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
;; functions written for testing.
   (define/public (for-test:x)  x)
   (define/public (for-test:selected?) selected?)
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-football:PosInt PosInt ->Toy<%>
;; GIVEN:initial x and y coordinates of target.
;; RETURNS:A Football object with Football placed at given x and y coordinates .
;; EXAMPLE:Check test for example.
(define (make-football initial-x initial-y)
  (new Football% [x initial-x][y initial-y][img FOOTBALL]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run : PosInt Int -> void
;; GIVEN: Frame Rate and speed with which rectangle moves.
;; RETURNS: a world with a wall, a ball, and a factory
;; DESIGN-STRATEGY:Combine Simpler Functions.
(define (run rate speed)
  (local
    ((define current-playground (make-Playground  speed )))
    (begin
    (send current-world add-stateful-widget current-playground)
    (send current-world run rate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a constant current target.
(define current-target (new Target% [x (/ CANVAS-WIDTH TWO)][y (/ CANVAS-HEIGHT TWO)]
               [selected? false]
               [saved-mx ZERO]
               [saved-my ZERO]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-Playground: ListOfToy Speed World-> Playground
;; GIVEN: a ListOfToy, a Target and the PosInt as travel speed
;; RETURNS: a WorldState initialized with the above parmeters
;; STRATEGY: call simpler function
(define (make-Playground  speed )
  (new Playground% [objs (list current-target)] [speed speed][world current-world]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define current-world (make-world CANVAS-WIDTH CANVAS-HEIGHT))
(define SQUARE1
  (new Square% [x 479][y 100][speed 20][selected? #f][saved-mx 0][saved-my 0]))
(define SQUARE2
  (new Square% [x 30][y 100][speed -20][selected? #f][saved-mx 0][saved-my 0]))
(define SQUARE3
  (new Square% [x 250][y 100][speed 10][selected? #f][saved-mx 0][saved-my 0]))
(define THROBBER1
  (new Throbber%
          [x 250][y 300][selected? #t][saved-mx 0][saved-my 0][count 0][radius 5]))
(define THROBBER2
  (new Throbber%
          [x 250][y 300][selected? #f][saved-mx 0][saved-my 0][count 0][radius 5]))
(define THROBBER3
  (new Throbber%
          [x 250][y 300][selected? #f][saved-mx 0][saved-my 0][count -1][radius 5]))
   
(define CLOCK1 (new Clock%
          [x 250][y 300][count 0][selected? #t][saved-mx 0][saved-my 0]))

(define CLOCK2 (new Clock%
          [x 250][y 300][count 0][selected? #f][saved-mx 0][saved-my 0]))
(define FOOTBALL1(new Football%
         [x 250][y 300][img FOOTBALL][selected? #f][saved-mx 0][saved-my 0][factor 0.1]))
(define FOOTBALL2 (new Football%
         [x 250][y 300][img FOOTBALL][selected? #f][saved-mx 0][saved-my 0][factor 0.1]))
(define FOOTBALL3 (new Football%
         [x 250][y 300][img FOOTBALL][selected? #t][saved-mx 0][saved-my 0][factor 0.9]))
(define TARGET1 (new Target% [x (/ CANVAS-WIDTH TWO)][y (/ CANVAS-HEIGHT TWO)]
               [selected? #f]
               [saved-mx ZERO]
               [saved-my ZERO]))

(define TARGET2 (new Target% [x (/ CANVAS-WIDTH TWO)][y (/ CANVAS-HEIGHT TWO)]
               [selected? #t]
               [saved-mx ZERO]
               [saved-my ZERO]))
(define PLAYGROUND (make-Playground  10 ))

(begin-for-test
;; Throbber%
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
     after-key-event "w") toy-y)300 "should return same throbber")
  (send THROBBER1 after-tick)
  (check-equal? (send THROBBER1 toy-x) 250
                "should return throbber expanded")
  (send THROBBER2 after-tick)
  (check-equal? (send THROBBER2 toy-data) 5
                "should return throbber expanded")
  (send THROBBER3 after-tick)
  (check-equal? (send THROBBER3 for-test:count) 1
                "should return throbber expanded")

  (send THROBBER2 after-button-down 600 600)
   (check-equal? (send THROBBER2 for-test:selected?)
   #f "Should be selected")
   (send THROBBER2 after-drag 475 100)
  (check-equal? (send THROBBER2 for-test:selected?)
   #f "Should be selected")
  (send CLOCK1 toy-y)
   (send THROBBER2 after-button-down 250 300)
  (check-equal? (send THROBBER2 for-test:selected?)
   #t "Should be selected")
  (send THROBBER2 after-drag 475 100)
  (check-equal? (send THROBBER2 for-test:selected?)
   #t "Should be selected")             
  (send THROBBER2 after-button-up 479 100)
  (check-equal? (send THROBBER2 for-test:selected?)
   #f "Should be selected")
 ;; (initial-world)
  (check-equal?
   (send THROBBER1 add-to-scene EMPTY-CANVAS)
   (place-image (circle 5 "solid" "green") 250 300 EMPTY-CANVAS)
   "render world")
  
(send PLAYGROUND after-key-event "s")
(send PLAYGROUND after-key-event "f")
(send PLAYGROUND after-key-event "w")
(send PLAYGROUND after-key-event "t")
(send PLAYGROUND after-button-down 250 300)
(send PLAYGROUND after-button-up 250 300)
(send PLAYGROUND after-drag 250 500)
(send PLAYGROUND after-key-event "g")
(send PLAYGROUND after-tick )
(send PLAYGROUND add-to-scene EMPTY-CANVAS)
(send PLAYGROUND target-selected?)
(send PLAYGROUND get-toys)
;; Square
  (check-equal?
   (send 
    (send
     (new Square% [x 200][y 100][speed 20][selected? #t][saved-mx 0][saved-my 0])
     after-tick) toy-x)
   200
   "should not move")
 (send
    SQUARE1
    after-tick)
 (check-equal?
  (send SQUARE1 
    toy-y)100   "should bounce")

 (send
    SQUARE3
    after-tick)
(check-equal?
  (send SQUARE3 
    toy-data)10   "should check toy-data") 

 (send SQUARE1 after-key-event "z")
 (check-equal? (send SQUARE1 for-test:x)
               480
               "should not change")
(send SQUARE2 after-tick)
  (check-equal?
   (send SQUARE2
     toy-x)
   20
   "should bounce")

  (check-equal?
   (send SQUARE2 add-to-scene EMPTY-CANVAS)
   (place-image (square SQUARE-SIDE "outline" "blue") 20 100 EMPTY-CANVAS)
   "render world")
   (send SQUARE1 after-button-down 600 600)
   (check-equal? (send SQUARE1 for-test:selected?)
   #f "Should be selected")
   (send SQUARE1 after-drag 475 100)
  (check-equal? (send SQUARE1 for-test:selected?)
   #f "Should be selected")        
   (send SQUARE1 after-button-down 479 100)
  (check-equal? (send SQUARE1 for-test:selected?)
   #t "Should be selected")
  (send SQUARE1 after-drag 475 100)
  (check-equal? (send SQUARE1 for-test:selected?)
   #t "Should be selected")             
  (send SQUARE1 after-button-up 479 100)
  (check-equal? (send SQUARE1 for-test:selected?)
   #f "Should be selected")
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
          [x 250][y 300][img FOOTBALL][selected? #f][saved-mx 0][saved-my 0][factor 0.7])
     after-key-event "w") toy-x) 250 "should stay the same")
  (check-equal?
   (send 
    (new Football%
         [x 250][y 300][img FOOTBALL][selected? #f][saved-mx 0][saved-my 0][factor 0.01])
    add-to-scene EMPTY-CANVAS) EMPTY-CANVAS "should render empty world")

  (check-equal? (send FOOTBALL1 toy-x) 250
                "should return throbber expanded")
  (send FOOTBALL1 after-tick)
  (check-equal? (send FOOTBALL1 toy-data) 64
                "should return throbber expanded")
  
  (send FOOTBALL2 toy-y)
  (send  FOOTBALL2 after-button-down 600 600)
   (check-equal? (send FOOTBALL2 for-test:selected?)
   #f "Should be selected")
   (send FOOTBALL2 after-drag 475 100)
  (check-equal? (send FOOTBALL2 for-test:selected?)
   #f "Should be selected")        
   (send FOOTBALL2 after-button-down 250 300)
  (check-equal? (send FOOTBALL2 for-test:selected?)
   #t "Should be selected")
  (send FOOTBALL2 after-drag 475 100)
  (check-equal? (send FOOTBALL2 for-test:selected?)
   #t "Should be selected")             
  (send FOOTBALL2 after-button-up 479 100)
  (check-equal? (send FOOTBALL2 for-test:selected?)
   #f "Should be selected")
  (check-equal?
   (send FOOTBALL3 add-to-scene EMPTY-CANVAS)
    (place-image FOOTBALL 250 300 EMPTY-CANVAS)
   "render world")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Target

  (send TARGET1 after-tick)
  (check-equal? (send TARGET1 for-test:x) 250
                "should return throbber expanded")
  
  (send TARGET1 after-key-event "z")

  (send TARGET1 after-button-down 600 600)
   (check-equal? (send TARGET1 for-test:selected?)
   #f "Should be selected")
   (send TARGET1 after-drag 475 100)
  (check-equal? (send TARGET1 for-test:selected?)
   #f "Should be selected")        
   (send TARGET1 after-button-down 250 300)
  (check-equal? (send TARGET1 for-test:selected?)
   #t "Should be selected")
  (send TARGET1 after-drag 475 100)
  (check-equal? (send TARGET1 for-test:selected?)
   #t "Should be selected")             
  (send TARGET1 after-button-up 479 100)
  (check-equal? (send TARGET1 for-test:selected?)
   #f "Should be selected")
  (check-equal?
   (send TARGET2 add-to-scene EMPTY-CANVAS)
   (place-image (circle TARGET-RADIUS "outline" "black") 250 300
                            EMPTY-CANVAS)
   "render world")
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
(send CLOCK1 after-tick)
  (check-equal? (send CLOCK1 toy-x) 250
                "should return throbber expanded")
  (send CLOCK2 after-tick)
  (send CLOCK2 after-tick)
  (check-equal? (send CLOCK2 toy-data) 2
                "should return throbber expanded")
  
  (send CLOCK2 after-button-down 600 600)
   (check-equal? (send CLOCK2 for-test:selected?)
   #f "Should be selected")
   (send CLOCK2 after-drag 475 100)
  (check-equal? (send CLOCK2 for-test:selected?)
   #f "Should be selected")        
   (send CLOCK2 after-button-down 250 300)
  (check-equal? (send CLOCK2 for-test:selected?)
   #t "Should be selected")
  (send CLOCK2 after-drag 475 100)
  (check-equal? (send CLOCK2 for-test:selected?)
   #t "Should be selected")             
  (send CLOCK2 after-button-up 479 100)
  (check-equal? (send CLOCK2 for-test:selected?)
   #f "Should be selected")
  (check-equal?
   (send CLOCK1 add-to-scene EMPTY-CANVAS)
   (place-image
    (overlay (square SQUARE-SIDE "outline" "blue")
             (text (number->string 0) 12 "black")) 250 300 EMPTY-CANVAS)
   "should render world"
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(run 0.2 10)