#lang racket

;;Implement a new  toy at factory to produce a new toy inspired by Cubelets,
;;which are square blocks that stick together.

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(provide make-block
         Block<%>)
(check-location "10" "cubelets.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define HALF 2)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
a ListOfBlock<%>(LOB) is a list which is either:
-- empty
-- (cons Block<%> LOB)

lob-fn: LOB -> ??
(define (lob-fn lob)
    (cond
        [(empty?) ...]
        [else ... (first lob)
              ... (lob-fn (rest lob))]))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The Block<%> interface extends the SWidget<%> interface with AT LEAST
;;the following methods:
(define Block<%>
  (interface (SWidget<%>)
    
    ;;get-team : -> ListOfBlock<%>
    ;;RETURNS: the teammates of this block
    get-team
    
    ;;add-teammate: Block<%> -> Void
    ;;EFFECT: adds the given block to this block's team
    add-teammate
    
    ;;block-x : -> Integer
    ;;block-y : -> Integer
    ;;RETURNS: the x or y coordinates of this block
    block-x
    block-y))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;make-block : NonNegInt NonNegInt ListOfBlock<%> -> Block<%>
;;GIVEN: an x and y position, and a list of blocks
;;WHERE: the list of blocks is the list of blocks already on the playground.
;;RETURNS: a new block, at the given position, with no teammates
;;NOTE: it is up to you as to whether you use the third argument or
;;not.  Some implementations may use the third argument; others may not.
;;STRATEGY: Generalization over new method
(define (make-block px py lst)
  (new Cubelet%
       [world void][x px][y py][saved-mx 0][saved-my 0][selected? false][loc lst]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cubelet% implements Block interface that describes the behavior of cubelet
;; cubelet is a (new Cubelet%
;;                 [x Int ][y Int]
;;                 [saved-mx Integer][saved-my Integer][selected? Boolean]
;;                 [loc ListOfBlocks<%>][world World])
(define Cubelet%
  (class* object% (Block<%>)
    ;; world in which cubelet lives, this field is used to retrieve the other cubelets
    ;; in the world
    (init-field world)
    
    ;; the x y position of the center of the cubelet
    (init-field x y)
    
    ;; the x y previous position of the center of the cursor
    (init-field saved-mx saved-my)
    
    ;; predicate telling if is selected
    (init-field selected?)
    
    ;; the ListOfCubelets(ie same as ListOfBlocks<%>)
    (init-field loc)
    
    ;; length of side of cubelet
    (field [square-side 20])
    
    ; image for displaying the when the cubelet is unselected
    (field [square-image-unselected (square square-side "outline" "green")])
    
    ; image for displaying the when the cubelet is selected
    (field [square-image-selected (square square-side "outline" "red")])
    
    (super-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; draw-right-image: -> Scene
    ;; RETURNS: a proper image of cubelet depending on if it is selected
    ;; STRATEGY: Cases on whether cubelet is selected or not. 
    (define (draw-right-image)
      (if selected?
          square-image-selected
          square-image-unselected))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; in-square? : PosInt PosInt -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside cubelet.
    ;; EXAMPLE:Check test for example
    ;; DESIGN-STRATEGY:Combine Simpler Functions.    
    (define (in-square?  cur-x cur-y)
      (and
       (<= 
        (- x (/ square-side HALF))
        cur-x
        (+ x (/ square-side HALF)))
       (<= 
        (- y (/ square-side HALF))
        cur-y
        (+ y (/ square-side HALF)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; othercubelets: -> ListOfBlock<%>
    ;; RETURNS: a list of Block without this cubelet and the factory
    ;; STRATEGY:Use HOF filter on ListOfBlock<%>
    (define (othercubelets)
      (local((define (sobjs-without-factory)
               (reverse(rest (reverse (dynamic-get-field 'sobjs world))))))
        (if (equal? world void)
            empty
            (filter (lambda(p) (not (equal? p this))) (sobjs-without-factory)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; bind-to-world: -> void
    ;; Effect: bind this cubelet to the world
    ;; EXAMPLE:Check test at end for example.
    ;; STRATEGY: Use simple function
    (define/public (bind-to-world mw)
      (set! world mw))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; get-team : -> ListOfBlock<%>
    ;; RETURNS: the teammates of this block
    ;; STRATERGY:Combine Simpler Functions
    (define/public (get-team) loc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; add-teammate: Cubelet% -> void
    ;; RETURNS:A teammate added to a block
    ;; EFFECT: adds the given block to this block's team.
    ;; EXAMPLE:Check test at end for example.
    ;; STRATERGY:Use HOF ormap on ListOfBlock<%> 
    (define/public (add-teammate c)
      (local(
             (define (present-in-list?)
               (ormap (lambda (cmp) (equal? c cmp)) loc)))
      (begin
        (if (present-in-list?)
            void
            (set! loc (cons c loc)))
        (send c add-teammate this))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; block-x : -> Integer
    ;; RETURNS: the x coordinates of this block
    ;; EXAMPLE:Check test for example
    ;; DESIGN-STRATERGY:Combine Simpler Functions
    (define/public (block-x) x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; block-y : -> Integer
    ;; RETURNS: the y coordinates of this block
    ;; EXAMPLE:Check test for example
    ;; DESIGN-STRATERGY:Combine Simpler Functions
    (define/public (block-y) y) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-tick: -> void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this widget to the state it should have
    ;; following a tick.
    ;; EXAMPLE:Check test for example.
    (define/public (after-tick) void)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-button-up: NonNegInt NonNegInt -> void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the specified mouse event at the given location.
    ;; EXAMPLE:Check test for example.
    ;; DESIGN-STRATERGY:Combine Simpler functions 
    (define/public (after-button-up mx my)
      (set! selected? false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-down: NoNegInt NonNegInt -> void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the specified mouse event at the given location.
    ;; EXAMPLE:Check test for example.
    ;; DESIGN-STRATERGY: cases on if mouse is in cubelet
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx mx)
            (set! saved-my my))
          void))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; move-along: Integer Integer -> void
    ;; GIVEN: the delta in movement of mouse
    ;; EFFECT: update the location of the cubelet.
    ;; EXAMPLE:Check test for example.
    ;; DESIGN-STRATEGY:Combine Simpler Functions.
    (define/public (move-along dx dy)
      (begin
        (set! x (+ x dx))
        (set! y (+ y dy))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; merge-buddy: Cubelet% -> void
   ;; GIVEN:A Block<%> to merge buddy
   ;; EFFECT:Merge the teammates of Block<%> with that of ours.
   ;; EXAMPLE:Check test for example
   ;; DESIGN-STRATEGY:Use HOF filter on ListOfBlock<%> 
    (define/public (merge-buddy c)
      (set! loc (filter (lambda(p) (not (equal? p this)))
              (set-union (send c get-team) loc))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; update-saved-position: Int Int -> void
  ;; Given:Mouse coordinates of a mouse event
  ;; EFFECT:It is use to update the saved mouse coordinates of teammates
  ;; EXAMPLE:Check test for example.
  ;; DESIGN-STRATEGY:Combine Simpler functions  
    (define/public (update-saved-position mx my)
      (set! saved-mx mx) ; update saved-mx saved-my
      (set! saved-my my))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; make-buddy-worldly: ->void
    ;; EFFECT: apply make-buddy on each element of in the list of Block<%>
    ;;         in the world where we live
    ;; STRATEGY: use HOF map on ListOfBlock<%>
    (define (make-buddy-worldly)
      (map (lambda (c) (make-buddy c)) (othercubelets)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; merge-buddy-mutually: -> void
    ;; EFFECT: do merge-buddy with other cubelets in the world and ask them to merge
    ;;         my teammates if there is contact
    ;; STRATEGY: use HOF map on ListOfBlock<%>
    (define (merge-buddy-mutually)
      (map (lambda (c) (merge-buddy c)) loc)
      (map (lambda (c) (send c merge-buddy this)) loc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; move-buddies: ListOfBlock<%> NonNegInt Integer Integer -> void
    ;; GIVEN: loc as ListOfBlock<%>, n as NonNegInt, dx and dy as Integer
    ;; EFFECT: push all non-selected teammates to move;
    ;;         if multiple cubelets selected at the same time, only send the distance
    ;;         that is devided by number of teammates selected at the same time
    ;; STRATEGY: use HOF map on ListOfBlock<%>
    (define (move-buddies loc n dx dy)
      (map
      ;; Block<%> -> void 
       (lambda (c)
             (if (not (dynamic-get-field 'selected? c))
                 (send c move-along
                       (/ dx n)
                       (/ dy n))
                 void)) loc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; count-selected: -> NonNegInt
    ;; RETURNS: number of members selected simultaneously
    ;; STRATEGY: use HOF foldr on ListOfBlock<%>
    (define (count-selected)
      (foldr
       (lambda(n s)
         (if (dynamic-get-field 'selected? n)
             (add1 s)
             s))
       1
       loc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-drag: Integer Integer -> void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the specified mouse event at the given location.
    ;; DESIGN-STRATEGY:Combine Simpler Functions
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (local(
                   (define delta-x (- mx saved-mx))
                   (define delta-y (- my saved-my)))
              (update-saved-position mx my)
              (move-along delta-x delta-y)
              (make-buddy-worldly)
              (merge-buddy-mutually)
              (move-buddies loc (count-selected) delta-x delta-y)
              ))
          void))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; make-buddy: Cubelet% -> void
    ;; GIVEN: a cubelet to make buddy with
    ;; EFFECT: add this as a buddy of the calling cubelet as well as add that back to one
    ;; of our buddy
    ;; EXAMPLE:Check test for example.
    ;; DESIGN STRATEGY:Combine Simpler Functions
    (define/public (make-buddy c)
      (if
       (and (<= (abs (- x (send c block-x))) square-side)
            (<= (abs (- y (send c block-y))) square-side))
       (begin
         (add-teammate c)
         )
       void))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-key-event : KeyEvent -> void
    ;; GIVEN: a key event
    ;; EFFECT: updates this widget to the state it should have
    ;; following the given key event
    ;; EXAMPLE:Check Test for example.
    (define/public (after-key-event key) void)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; add-to-scene:Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    ;; STRATEGY: call simpler function
    (define/public (add-to-scene s)
      (place-image 
       (draw-right-image) x y s))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cubeletFactory Implements SWidget interface
;; CubeletFactory is a(new CubeletFactory% [world World])

(define CubeletFactory% 
  (class* object% (SWidget<%>)
    
    (init-field world)  ; the world to which the factory adds cubelets
    ;; Are the saved x and y mouse coordinates of cubelets
    (field [saved-mx (/ CANVAS-WIDTH HALF)])
    (field [saved-my (/ CANVAS-HEIGHT HALF)])
    
    (super-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bind-to-world : -> void
;; Effect:It binds the cubelet to statefull world
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Combine Simpler Functions    
    (define/public (bind-to-world)
      (send world add-stateful-widget this))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-key-event: KeyEvent -> void
;; Given:A keyevent to add a cubelet
;; Effect:Adds a cubelet when b is pressed.
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Use cases on KeyEvents.    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "b")
         (local((define newblock
                  (make-block 
                   saved-mx
                   saved-my
                   empty))
                (define (newblock-with-world)
                  (send newblock bind-to-world world)))         
           (begin
             (newblock-with-world)
             (send world add-stateful-widget newblock))
           ;newblock-with-world
           )]
        [else void]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-tick:->void
    ;; Effect:It has no effect on cubelet class.
    ;; EXAMPLE:Check test for example.
    (define/public (after-tick) void)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-down:NonNegInt NonNegInt -> void
    ;; Effect:It is used to set the saved x and y coordinates of cubelet.
    ;; Example:Check test for example
    ;; DESIGN-STRATEGY:Combine Simpler Functions.
    (define/public (after-button-down mx my)
      (begin 
        (set! saved-mx mx)
        (set! saved-my my)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
   ;; after-button-up:NonNegInt NonNegInt -> void
   ;; Effect:It is used to set the saved x and y coordinates of cubelet.
   ;; EXAMPLE:Check test for example
   ;; DESIGN-STRATEGY:Combine simpler functions.  
    (define/public (after-button-up mx my)
      (begin 
        (set! saved-mx mx)
        (set! saved-my my)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; after-drag:Int Int -> void
   ;; Effect:This functions does nothing.
    (define/public (after-drag mx my) void)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; add-to-scene: Scene->Scene
    ;; Given:A scene.
    ;; Returns:A scene as before one.
    ;; EXAMPLE:Check test for example.
    (define/public (add-to-scene s) s))
  
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial-world: -> void
;; Effect: creates a world and adds cubelet in world
;; STRATEGY:Combine Simpler Functions
;; EXAMPLE:Check test for example.
(define (initial-world)
  (local
    ((define myworld
       (make-world CANVAS-WIDTH CANVAS-HEIGHT))
     (define mycubeletfactory
       (new CubeletFactory%
            [world myworld])))
    (begin
      (send mycubeletfactory bind-to-world)
      myworld)))
;(define obj (initial-world))
;(send obj run 0.2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TEST SUITE
(begin-for-test
  (local ((define obj (initial-world))
          )
    (check-equal?
     (length (dynamic-get-field 'sobjs obj))
    1)
    (send (first (dynamic-get-field 'sobjs obj)) after-key-event "b");factory
    (local ((define myworld1 obj)
            (define factory1 (second (dynamic-get-field 'sobjs obj))))
      (check-equal?  
       (length (dynamic-get-field
                'sobjs
                obj))        
       2)

      (send factory1 after-key-event "t")
      (send factory1 after-tick)
      (send factory1 after-drag 100 300)
      (check-equal?
       obj
       myworld1)
      (check-equal?  
       (send factory1 add-to-scene EMPTY-CANVAS)
       EMPTY-CANVAS)
       
      )
    
    (send (second (dynamic-get-field 'sobjs obj)) after-button-down 100 100);factory
    (send (second (dynamic-get-field 'sobjs obj)) after-button-up 100 100);factory
    (send (second (dynamic-get-field 'sobjs obj)) after-key-event "b");factory
    (local (
            (define myworld2 obj)
            (define cube (first
                          (dynamic-get-field
                           'sobjs
                           obj))))
      (check-equal?  
       (length (dynamic-get-field
                'sobjs
                obj))
       3)
      (check-equal?
       (send cube add-to-scene EMPTY-CANVAS)
       (place-image (square 20 "outline" "green") 100 100 EMPTY-CANVAS)))
    (send (first (dynamic-get-field 'sobjs obj)) after-button-down 100 100);cube2
    (local (
            (define myworld2 obj)
            (define cube (first
                          (dynamic-get-field
                           'sobjs
                           obj))))
      (check-equal?  
       (length (dynamic-get-field
                'sobjs
                obj))
       3)
      (check-equal?
       (send cube add-to-scene EMPTY-CANVAS)
       (place-image (square 20 "outline" "red") 100 100 EMPTY-CANVAS))
      (send cube after-key-event "b")
      (check-equal?
       cube
       (first
        (dynamic-get-field
         'sobjs
         obj)))
      )
    ;(send (first (dynamic-get-field 'sobjs obj)) after-button-up 100 100); cube2
    (send (first (dynamic-get-field 'sobjs obj)) after-drag 110 100); cube2
    (send (third (dynamic-get-field 'sobjs obj)) after-button-down 110 100); factory
    (send (third (dynamic-get-field 'sobjs obj)) after-key-event "b"); factory
    (send (fourth (dynamic-get-field 'sobjs obj)) after-key-event "b"); factory
    (send (third (dynamic-get-field 'sobjs obj)) after-drag 100 100); cube2
    (send (first (dynamic-get-field 'sobjs obj)) after-button-down 100 100); cube4
    (send (first (dynamic-get-field 'sobjs obj)) after-drag 105 100); cube4   
    (local (
            (define myworld3 obj)
            (define cube4 (first
                          (dynamic-get-field
                           'sobjs
                           obj)))
            (define cube3 (second
                          (dynamic-get-field
                           'sobjs
                           obj)))
            (define cube2 (third
                          (dynamic-get-field
                           'sobjs
                           obj)))
            (define cube1 (fourth
                          (dynamic-get-field
                           'sobjs
                           obj))))
      (check-equal?  
       (length (dynamic-get-field
                'sobjs
                obj))
       5)
      (check-equal?
       (dynamic-get-field 'selected? cube2)
       #t
      )
      (send cube4 after-button-up 110 100)
      (check-equal?
       (first (dynamic-get-field 'sobjs obj))
       cube4)
    )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define obj (initial-world))
;(send obj run 0.2)