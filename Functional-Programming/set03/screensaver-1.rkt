;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;GOAL:To implement a screensaver
(check-location "03" "screensaver-1.rkt")
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 world-rect1
 world-rect2
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; screensaver

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world
;; DESIGN-STRATERGY:Combine Simpler Function
(define (screensaver speed-of-simulation)
  (big-bang (initial-world speed-of-simulation)
            (on-tick world-after-tick  speed-of-simulation)
            (on-key world-after-key-event)
            (to-draw world-to-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CONSTANTS
;;Initial Coordinates of rectangle.
(define RECT1-X 200)
(define RECT1-Y 100)
(define RECT2-X 200)
(define RECT2-Y 200)

;;INITIAL VELOCITIES OF TWO RECTANGLES
(define VX1  (- 12))
(define VY1  20)
(define VX2 23)
(define VY2 (- 14))
;;DEFINE TEXT FONT
(define FONT-SIZE 10)
(define BLUE "blue")

;;Dimensions of Canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define RECT (rectangle 60 50 "outline" "blue"))
(define LOWER-BOUNDARY (- CANVAS-HEIGHT (/ (image-height RECT) 2)))
(define RIGHT-BOUNDARY (- CANVAS-WIDTH  (/ (image-width RECT) 2)))
(define UPPER-BOUNDARY (/ (image-height RECT) 2)) 
(define LEFT-BOUNDARY (/ (image-width RECT) 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA DEFINATIONS

(define-struct world(rect1 rect2 paused?))

;; A WorldState  is a (make-world  rectangle rectangle boolean)
;;rect1 and rect2 are two rectangles
;;paused? describes if the world is paused or not
;;TEMPLATE:
;;world-fn :world ->??
;;(define (world-fn w)
;;(.....(world-rect1 w) (world-rect2 w) (world-paused? w)))


(define-struct rect(x y vx vy))

;;A rectangle is a (make-rect posnum posnum int int)
;;INTERPRETATION:
;;x,y are the x and y coordinate of center of rectangle
;;vx and vy are the velocities with which they are moving in x and y direction respectively.
;;TEMPLATE:
;;rectangle-fn: rectangle->??
;;(define (rectangle-fn r)
;;(....(rectangle-x-pos r) (rectangle-y-pos r) (rectangle-mx) (rectangle-my)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TEST-CASE CONSTANT
(define RECT1 (make-rect RECT1-X RECT1-Y VX1 VY1))
(define RECT1-AFTER-TICK (make-rect (+ RECT1-X VX1) (+ RECT1-Y VY1) VX1 VY1))
(define RECT2 (make-rect RECT2-X RECT2-Y VX2 VY2))
(define COORDINATE 100)
(define RECT-AT-LT-CORNER
  (make-rect  LEFT-BOUNDARY  UPPER-BOUNDARY  VX1 (- VY1)))
(define RECT-AF-LT-CORNER
  (make-rect  LEFT-BOUNDARY  UPPER-BOUNDARY (- VX1) VY1))
(define RECT-AT-RT-CORNER
  (make-rect  RIGHT-BOUNDARY UPPER-BOUNDARY (- VX1)(- VY1)))
(define RECT-AF-RT-CORNER
  (make-rect RIGHT-BOUNDARY  UPPER-BOUNDARY VX1  VY1 ))
(define RECT-AT-LB-CORNER
  (make-rect  LEFT-BOUNDARY   LOWER-BOUNDARY VX1  VY1))
(define RECT-AF-LB-CORNER
  (make-rect  LEFT-BOUNDARY   LOWER-BOUNDARY (- VX1)(- VY1)))
(define RECT-AT-RB-CORNER
  (make-rect  RIGHT-BOUNDARY  LOWER-BOUNDARY(- VX1) VY1))
(define RECT-AF-RB-CORNER
  (make-rect  RIGHT-BOUNDARY  LOWER-BOUNDARY   VX1 (- VY1)))
(define SIZE 10)
(define VELOCITY "(-12,20)")
(define RECT-INITIAL
  (make-rect RECT1-X RECT1-Y  VX1 VY1))
(define RECT-INITIAL1
  (make-rect RECT2-X RECT2-Y  VX2 VY2))
(define INITIAL-WORLD
  (make-world RECT-INITIAL RECT-INITIAL1 true))
(define RECT-AT-LW-BOUNDRY
  (make-rect COORDINATE LOWER-BOUNDARY VX1 VY1))
(define RECT-AF-LW-BOUNDRY
  (make-rect (+ COORDINATE VX1)LOWER-BOUNDARY VX1 (- VY1)))
(define RECT-AT-RB-BOUNDRY
  (make-rect RIGHT-BOUNDARY  COORDINATE (- VX1) VY1))
(define RECT-AF-RB-BOUNDRY
  (make-rect RIGHT-BOUNDARY (+ COORDINATE VY1) VX1  VY1))
(define RECT-AT-UB-BOUNDRY
  (make-rect COORDINATE  UPPER-BOUNDARY VX1 (- VY1)))
(define RECT-AF-UB-BOUNDRY
  (make-rect (+ COORDINATE VX1)  UPPER-BOUNDARY VX1 VY1))
(define RECT-AT-LB-BOUNDRY
  (make-rect LEFT-BOUNDARY   COORDINATE VX1  VY1))
(define RECT-AF-LB-BOUNDRY
  (make-rect LEFT-BOUNDARY  (+ COORDINATE VY1)(- VX1)  VY1))
(define UNPAUSED-WORLD (make-world RECT1 RECT2 false))
(define PAUSED-WORLD (make-world RECT1 RECT2 true))
(define WORLD1
  (make-world RECT-AT-LB-BOUNDRY RECT-AT-LB-BOUNDRY false))
(define WORLD2
  (make-world RECT-AF-LB-BOUNDRY RECT-AF-LB-BOUNDRY false))
(define RECT-INITIAL3
  (make-rect RECT2-X RECT2-Y  VX1 VY1))
(define RECTIMAGE1 (overlay  RECT
                  (text VELOCITY SIZE "blue")))
(define SCENE-WORLD
  (make-world RECT-INITIAL RECT-INITIAL3 false))

;;Represents a pause key event
(define pause-key-event? " ")
(define unpaused-key-event? "q")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : WorldState -> WorldState
;; GIVEN: a worldstate w
;; RETURNS: the world that should follow w after a tick.
;; STRATEGY: Use template for WorldState on w
(define (world-after-tick w)
  (if
   (world-paused? w)
   w
   (make-world
    (rect-after-tick (world-rect1 w))
    (rect-after-tick (world-rect2 w))
    (world-paused? w))))

;;TEST
(begin-for-test
  (check-equal?
   (world-after-tick  PAUSED-WORLD) PAUSED-WORLD
   "The Rectangle should not move in paused world")
  (check-equal?
   (world-after-tick  WORLD1)WORLD2
   "The Rectangle should not move in  world"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rect-after-tick:Rectangle -> Rectangle
;;GIVEN:A rectangle before the clock tick
;;RETURNS:A rectangle after clock tick
;;EXAMPLE:Check test for example
;;DESIGN STRATERGY:Combine Simpler Functions
(define (rect-after-tick rect)
  (cond
    [(check-for-corner-collision? rect)
     (change-velocities-after-corner-hit rect)]                                 
    [(check-for-boundary-collision? rect)
     (change-velocity-after-boundry-hit rect)]                                             
    [else (noboundary-collision rect)]))

;;Test:
(begin-for-test  
  (check-equal? (rect-after-tick RECT1) RECT1-AFTER-TICK
                "The position of rectangle after first tick")
   (check-equal? (rect-after-tick RECT-AT-LT-CORNER) RECT-AF-LT-CORNER
                "The velocities should reverse after Left top corner hit")
   (check-equal? (rect-after-tick RECT-AT-LW-BOUNDRY) RECT-AF-LW-BOUNDRY
                "The velocities should reverse after Left top corner hit"))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;check-for-corner-collision:Rectangle->boolean
;;Given:A rectangle to check if it is colliding in one of the corners
;;RETURNS:A boolean value which returns true if given rectangle collides with corner
;;EXAMPLE:Check Test of rect-after-tick
;;DESIGN STRATERGY:Combine Simpler Function
(define (check-for-corner-collision? rect)
  (or
   (will-collide-lefttop-corner? rect)
   (will-collide-righttop-corner? rect) 
   (will-collide-leftbottom-corner? rect) 
   (will-collide-rightbottom-corner? rect)))

(begin-for-test
  (check-equal? (check-for-corner-collision? RECT-AT-LT-CORNER) #T
                "Should return true as hitting corner")
  (check-equal? (check-for-corner-collision? RECT-AT-RT-CORNER) #T
                "Should return true as hitting corner")
  (check-equal? (check-for-corner-collision? RECT-AT-LB-CORNER) #T
                "Should return true as hitting corner")
  (check-equal? (check-for-corner-collision? RECT-AT-RB-CORNER) #T
                "Should return true as hitting corner"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;check-for-boundary-collision:Rectangle->boolean
;;GIVEN:A rectangle to check if rectangle is colliding with one of boundary wall.
;;RETURNS:A boolean value which returns true if
;;EXAMPLE:Check test for example
;;DESIGN STRATERGY:Combine Simpler Function
(define (check-for-boundary-collision? rect)
  (or
   (< LOWER-BOUNDARY (+ (rect-y rect) (rect-vy rect)))
   (< RIGHT-BOUNDARY (+ (rect-x rect) (rect-vx rect)))
   (> UPPER-BOUNDARY (+ (rect-y rect) (rect-vy rect)))
   (> LEFT-BOUNDARY (+ (rect-x rect) (rect-vx rect)))))

;;TEST
(begin-for-test
  (check-equal? (check-for-boundary-collision? RECT-AT-LW-BOUNDRY) #T
                "Should return true as hitting lower boundry")
  (check-equal? (check-for-boundary-collision? RECT-AT-RB-BOUNDRY) #T
                "Should return true as hitting right boundry")
  (check-equal? (check-for-boundary-collision? RECT-AT-UB-BOUNDRY) #T
                "Should return true as hitting upper boundry")
  (check-equal? (check-for-boundary-collision? RECT-AT-LB-BOUNDRY) #T
                "Should return true as hitting left boundry"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;changle-velocities-after-corner-hit:Rectangle->Rectangle
;;GIVEN:A rectangle which collides in the corner.
;;RETURNS:A rectangle with its both velocities reversed .
;;EXAMPLE:Check test for example.
;;DESIGN STRATERGY:Combine Simpler Functions
(define (change-velocities-after-corner-hit rect)
  (cond
    [(will-collide-lefttop-corner? rect)
     (reverse-both-velocities rect LEFT-BOUNDARY UPPER-BOUNDARY)]
    [(will-collide-righttop-corner? rect)
     (reverse-both-velocities rect RIGHT-BOUNDARY UPPER-BOUNDARY)]
    [(will-collide-leftbottom-corner? rect)
     (reverse-both-velocities rect LEFT-BOUNDARY LOWER-BOUNDARY)]
    [(will-collide-rightbottom-corner? rect)
     (reverse-both-velocities rect RIGHT-BOUNDARY LOWER-BOUNDARY)]))

;;TEST
(begin-for-test
  (check-equal? (change-velocities-after-corner-hit RECT-AT-LT-CORNER)
                RECT-AF-LT-CORNER
                "The velocities should reverse after Left top corner hit")
  (check-equal? (change-velocities-after-corner-hit RECT-AT-RT-CORNER)
                RECT-AF-RT-CORNER
                "The velocities should reverse after right top corner hit")
  (check-equal? (change-velocities-after-corner-hit RECT-AT-LB-CORNER)
                RECT-AF-LB-CORNER
                "The velocities should reverse after right top corner hit")
  (check-equal? (change-velocities-after-corner-hit RECT-AT-RB-CORNER)
                RECT-AF-RB-CORNER
                "The velocities should reverse after right bottom corner hit"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;change-velocity-after-boundry-hit:Rectangle -> Rectangle
;;GIVEN:A rectangle which collides with one of the boundaries.
;;RETURNS:A rectangle with one of the boundaries reversed after collision
;;EXAMPLE:Check Test for example.
;;DESIGN STRATERGY:Combine Simpler Functions
(define (change-velocity-after-boundry-hit rect)
  (cond
    [(< LOWER-BOUNDARY (+ (rect-y rect) (rect-vy rect)))
     (reverse-velocity-after-horizontal-collision rect LOWER-BOUNDARY)]
    [(< RIGHT-BOUNDARY (+ (rect-x rect) (rect-vx rect)))
     (reverse-velocity-after-vertical-collision rect RIGHT-BOUNDARY)]
    [(> UPPER-BOUNDARY (+ (rect-y rect) (rect-vy rect)))
     (reverse-velocity-after-horizontal-collision rect UPPER-BOUNDARY)]
    [(> LEFT-BOUNDARY (+ (rect-x rect) (rect-vx rect)))
     (reverse-velocity-after-vertical-collision rect  LEFT-BOUNDARY)]))

;;TEST
(begin-for-test
  (check-equal? (change-velocity-after-boundry-hit RECT-AT-LW-BOUNDRY) RECT-AF-LW-BOUNDRY
                "The velocity should reverse after bottom hit")
  (check-equal? (change-velocity-after-boundry-hit RECT-AT-RB-BOUNDRY) RECT-AF-RB-BOUNDRY
                "The velocity should reverse after right boundary hit")
  (check-equal? (change-velocity-after-boundry-hit RECT-AT-UB-BOUNDRY) RECT-AF-UB-BOUNDRY
                "The velocity should reverse after upper boundary hit")
  (check-equal? (change-velocity-after-boundry-hit  RECT-AT-LB-BOUNDRY)RECT-AF-LB-BOUNDRY
                "The velocity should reverse after left boundary hit"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;will-collide-leftop-corner?:Rectangle->boolean
;;RETURNS:A boolean value as true if rectangle collides in the leftop corner
;;EXAMPLE:Check the test condition of check-for-corner-collision? function
;;DESIGN-STRATERGY:Combine Simpler Functions
(define (will-collide-lefttop-corner? rect)
  (and (> LEFT-BOUNDARY (+ (rect-x rect)(rect-vx rect)))
       ( > UPPER-BOUNDARY (+(rect-y rect)(rect-vy rect)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;will-collide-leftbottom-corner?:Rectangle->boolean
;;RETURNS:A boolean value as true if rectangle collides in the leftbottom corner
;;EXAMPLE:Check the test condition of check-for-corner-collision? function
;;DESIGN-STRATERGY:Combine Simpler Functions
(define (will-collide-leftbottom-corner? rect)
  (and (> LEFT-BOUNDARY (+ (rect-x rect)(rect-vx rect)))
       (< LOWER-BOUNDARY (+ (rect-y rect)(rect-vy rect)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;will-collide-righttop-corner?:Rectangle->boolean
;;RETURNS:A boolean value as true if rectangle collides in the righttop corner
;;EXAMPLE:Check the test condition of check-for-corner-collision? function
;;DESIGN-STRATERGY:Combine Simpler Functions
(define (will-collide-righttop-corner? rect)
  (and (> UPPER-BOUNDARY (+ (rect-y rect)(rect-vy rect)))
       (< RIGHT-BOUNDARY (+ (rect-x rect)(rect-vx rect)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;will-collide-rightbottom-corner?:Rectangle->boolean
;;RETURNS:A boolean value as true if rectangle collides in the rightbottom corner
;;EXAMPLE:Check the test condition of check-for-corner-collision? function
;;DESIGN-STRATERGY:Combine Simpler Functions
(define (will-collide-rightbottom-corner? rect)
  (and (> (+ (rect-y rect)(rect-vy rect)) LOWER-BOUNDARY )
       (>  (+ (rect-x rect)(rect-vx rect)) RIGHT-BOUNDARY )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;reverse-both-velocities:Rectangle NonNegINT NonNegINT->Rectangle
;;GIVEN:A rectangle and the horizontal and vertical boundry limit.
;;RETURNS:A rectangle with both its velocities reversed.
;;EXAMPLE:Check test of change-velocities-after-corner-hit function
;;DESIGN STRATERGY:Combine Simpler Functions
(define (reverse-both-velocities rect boundary-x boundary-y)
  (new-rectangle (+ (rect-x rect) (- boundary-x (rect-x rect)))
                 (+ (rect-y rect) (- boundary-y (rect-y rect)))
                 (- (rect-vx rect)) (- (rect-vy rect))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;noboundary-collision:Rectangle->Rectangle
;;GIVEN:A rectangle travelling on the canvas
;;RETURNS:A rectangle after moving by the given velocities in a single clock tick
;;DESIGN STRATERGY:Combine Simpler Functions
(define (noboundary-collision rect)
  (new-rectangle (+ (rect-x rect)(rect-vx rect))
                 (+ (rect-y rect)(rect-vy rect))
                 (rect-vx rect) (rect-vy rect)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;reverse-velocity-after-horizontal-collision:Rectangle NonNegInt ->Rectangle
;;GIVEN:A rectangle going to collide with one of the horizontal boundaries.
;;RETURNS:A rectangle with velocity along y-axis reversed.
;;EXAMPLE:Check Test for example of change-velocities-after-boundary-hit function
;;DESIGN STRATERGY:Combine Simpler Functions
(define (reverse-velocity-after-horizontal-collision rect boundary)
  (new-rectangle (+ (rect-x rect) (rect-vx rect))
                 (+ (rect-y rect) (- boundary (rect-y rect)))
                 (rect-vx rect)  (- (rect-vy rect))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;reverse-velocity-after-vertical-collision:Rectangle NonNegInt->Rectangle
;;GIVEN:A rectangle going to collide with one of the vertical boundaries.
;;RETURNS:A rectangle with velocity along x-axis reversed.
;;EXAMPLE:Check Test Check Test for example of change-velocities-after-boundary-hit function
;;DESIGN STRATERGY:Combine Simpler Functions
(define (reverse-velocity-after-vertical-collision rect boundary)
  (new-rectangle (+ (rect-x rect) (- boundary (rect-x rect)))
                 (+ (rect-y rect) (rect-vy rect))
                 (- (rect-vx rect))  (rect-vy rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below for example
;; STRATEGY: cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")(world-with-paused-toggled w)]
    [else w]))

;;TEST
(begin-for-test
  (check-equal?
    (world-after-key-event UNPAUSED-WORLD pause-key-event?)
    PAUSED-WORLD
    "after pause key, a paused world should become unpaused")
(check-equal?
    (world-after-key-event PAUSED-WORLD pause-key-event?)
    UNPAUSED-WORLD
    "after pause key, an unpaused world should become paused")
(check-equal?
    (world-after-key-event PAUSED-WORLD unpaused-key-event?)
    PAUSED-WORLD
    "after unpause key, an paused world should remain paused"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-paused-toggled : WorldState -> WorldState
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: use template for WorldState on w
(define (world-with-paused-toggled w)
  (make-world (world-rect1 w)
              (world-rect2 w)
              (not (world-paused? w))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-rectangle-with-text:Integer Integer->Scene
;; GIVEN:Velocities of rectangle
;; RETURNS:A Rectangle with velocities represented as
;; textual representation overlayed on rectangle.
;; DESIGN STRATERGY:Combine Simpler Functions
(define (create-rectangle-with-text vx vy)
  (overlay RECT
           (text (convert-velocities-tostring vx vy) FONT-SIZE "blue")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert-velocities-tostring Integer Integer->String
;; GIVEN:Velocities of rectangle
;; RETURNS:Convert velocities to string
;; EXAMPLE (convert-velocities-tostring 12 20)=(12,20)
;;DESIGN STRATERGY:Combine Simpler Functions
(define(convert-velocities-tostring vx vy)
  (string-append
   "(" (number->string vx) "," (number->string vy) ")"))
;;TEST:
(begin-for-test
  (check-equal? (convert-velocities-tostring 12 20) "(12,20)"
                "Should convert Velocities into string format and store as above"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-to-scene : WorldState -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene paused-world-at-20) should return a canvas with
;; two rectangles, one at (150,20) and one at (300,28)
;;          
;; STRATEGY: Use template for WorldState on w
(define (world-to-scene w)
  (place-rectangle
   (world-rect1 w)
   (place-rectangle
    (world-rect2 w)
    EMPTY-CANVAS)))

;;TEST
(begin-for-test
  (check-equal? (world-to-scene SCENE-WORLD)
              (place-image RECTIMAGE1 RECT1-X RECT1-Y (place-image  RECTIMAGE1  RECT2-X RECT2-Y EMPTY-CANVAS))
      "Returns a rectangle"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-rectangle : Rectangle Scene -> Scene
;; RETURNS: a scene like the given one, but with the given rectangle painted
;; on it.
;; DESIGN STRATERGY:Combine Simpler Function
(define (place-rectangle rect s)
  (place-image
   (create-rectangle-with-text (rect-vx rect) (rect-vy rect))
   (rect-x rect) (rect-y rect)
   s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; DESIGN STRATERGY:Combine Simpler Function
(define (new-rectangle x-pos y-pos  vx vy)
  (make-rect  x-pos   y-pos vx vy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Integer -> WorldState
;; RETURNS: the initial world specified in the problem set.
;; DESIGN STRATERGY:Combine Simpler Function.
(define (initial-world speed-simulation)
  (make-world
   (make-rect RECT1-X RECT1-Y  VX1 VY1)
   (make-rect RECT2-X RECT2-Y  VX2 VY2)  
   true ))
;;TEST

(begin-for-test
 (check-equal? (initial-world 0.2) INITIAL-WORLD
    "Should return the initial world"))


;;(screensaver 0.5)
