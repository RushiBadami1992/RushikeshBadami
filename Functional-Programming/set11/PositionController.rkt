#lang racket

;; displays as an outline rectangle with text showing the x
;; coordinate and velocities of the particle.

;; the rectangle is draggable



(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require "controller.rkt")
(require rackunit)
(require "extras.rkt")
(require "Model.rkt") ; for test
(require "constants.rkt")
(provide PositionController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a PositionController is a (new PositionController% [model Model<%>][x Int][y Int]
;;                      [selected? boolean][saved-mx Int][saved-my Int][HANDLE-IMG IMG]
;;                      [handle-side Int])


(define PositionController%
  (class* Controller% ()

    (inherit-field model)  ; the model

    ;  the position of the center of the controller
    (inherit-field x)
    (inherit-field y)   

    ; default size, if text becomes too small, maintain this size
    ; to beautify the controller
    (init-field [width 120][height 50])
    ; Use to define half canvas-width of the canvas
    (field [half-width  (/ width  HALF)])
    ; Defines  half canvas-height of the canvas
    (field [half-height (/ height HALF)])

    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    (inherit-field selected?)
    ;; Checks if text in position controller is selected or not
    (field [text-selected? false])
    ;;;; the last button-down event inside the position controller, relative to the
    ;; position controllers center.  Else any value.
    (inherit-field saved-mx)
    (inherit-field saved-my)
    ;; this field defines the handle image
    (inherit-field HANDLE-IMG)
    ;; this field defines the handle side
    (inherit-field handle-side)

    (super-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (set! x MAX-WIDTH)
    (set! y MAX-HEIGHT)
    (set! saved-my INITIAL-POINT)
    (set! saved-mx INITIAL-POINT)
    (set! selected? false)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; real-width:-> Int
  ;; RETURNS:A integer which is the width of image
  ;; EXAMPLE:Check test for example
  ;; DESIGN-STRATEGY:Combine Simpler functions  
    (define (real-width) (max width (+ (image-width (data-image)) TEXT-SIZE)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; real-height:-> Int
;; RETURNS:A integer which is the height of image
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Combine Simpler functions    
    (define (real-height) (max height (+ (image-height (data-image)) TEXT-SIZE)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; controller-selected? NonNegInt NonNegInt -> Boolean
;; Given:The x and y coordinates of mouse
;; RETURNS:If the handle is selected or not
;; EXAMPLE:Check test for example.
;; DESIGN-STRATEGY:Combine Simpler Functions    
    (define/override (controller-selected? other-x other-y)
      (local((define the-data-image  (data-image))
             (define handle-x ( + x (- (/ handle-side HALF) (/ (real-width) HALF))))
             (define handle-y ( + y (- (/ handle-side HALF) (/ (real-height) HALF)))))
      (and
        (<= (abs (- other-x handle-x))  (/ handle-side HALF))
        (<= (abs (- other-y handle-y))  (/ handle-side HALF)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-key-event:KeyEvent-> Void
;; GIVEN:A key-event to change the position of x or y coordinate of particle
;; EXAMPLE:Check test for example.
;; DESIGN-STRATEGY:Use cases on Key events.    
    (define/override (after-key-event kev)
      (if text-selected?
        (local((define my-p (send model get-p)))
            (cond
            [(key=? LEFT kev)
             (send model execute-command
               (make-particle (- (particle-x my-p) INCREASE-VELOCITY)
                              (particle-y my-p) (particle-vx my-p)
                              (particle-vy my-p)))]
            [(key=? RIGHT kev)
             (send model execute-command
               (make-particle (+ (particle-x my-p) INCREASE-VELOCITY)
                              (particle-y my-p) (particle-vx my-p) 
                              (particle-vy my-p)))]
            [(key=? UP kev)
             (send model execute-command
               (make-particle (particle-x my-p)
                              (- (particle-y my-p) INCREASE-VELOCITY) (particle-vx my-p) 
                              (particle-vy my-p)))]
            [(key=? DOWN kev)
             (send model execute-command
               (make-particle (particle-x my-p) (+ (particle-y my-p) 5) (particle-vx my-p) 
                              (particle-vy my-p)))]
            ))
        void))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current-color:Boolean-> String
;; GIVEN:A true or false to retun color to be rendered for text
;; RETURNS:A color of string accordingly
;; EXAMPLE:Check Test for Example
;; DESIGN-STRATEGY:Combine Simpler Functions.    
    (define (current-color predicate)
      (if predicate RED BLACK))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-down:NonNegInt NonNegInt -> void
;; Effect:Alter the state of selected and text selected.
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:use cases on if text-selected or not    
    (define/override (after-button-down mx my)
      (local((define half-real-width (/ (real-width) HALF))
             (define half-real-height (/ (real-height) HALF))
             (define (is-text-selected?)
                (and
                    (<= (- x half-real-width) mx (+ x half-real-width))
                    (<= (- y half-real-height) my (+ y half-real-height)))))
      (begin 
        (super after-button-down mx my)
        (if (and (is-text-selected?) (not selected?))
            (set! text-selected? true)
            void))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-up :NonNegInt NonNegInt->void
;; Effect:Alter the state of selected? and text-selected?
;; EXAMPLE:Check Test for example
;; DESIGN-STRATEGY:Combine Simpler Functions    
    (define/override (after-button-up mx my)
        (super after-button-up mx my)
        (set! text-selected? false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; render-image -> Image
;; RETURN:An image of position controller
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Combine Simpler Functions    
  
    (define/override (render-my-image)
      (local ((define the-data-image (data-image)))
        (overlay/xy
            (square handle-side OUTLINE (current-color selected?))
            INITIAL-POSITION
            INITIAL-POSITION
        (overlay 
          the-data-image
          (rectangle
            (real-width)
            (real-height)
            OUTLINE 
            BLACK)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; data-image -> Image
;; RETURN:An image of text of position controller
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Combine Simpler Functions    
    
 
    (define (data-image)
      (local((define my-p (send model get-p)))
      (above
        (text "  Arrow keys change position" 8 (current-color text-selected?))
        (text (string-append
                "X = "
                (number->string (round-decimal(particle-x my-p)))
                " "
                "Y = "
                (number->string (round-decimal(particle-y my-p))))
              TEXT-SIZE
           (current-color text-selected?))
        (text (string-append
                "VX = "
                (number->string (round-decimal(particle-vx my-p)))
                " "
                "VY = "
                (number->string (round-decimal(particle-vy my-p))))
          TEXT-SIZE
          (current-color text-selected?)))))

    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; round-decimal:Num->Num
;; GIVEN:X,Y,vx,vy coordinate of particle
;; RETURNS:A number upto to decimal precision
;; DESIGN-STRATEGY:Combine Simpler Functions
(define (round-decimal x)
  (/ (round(* (exact->inexact x) 100)) 100))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (local((define m (new Model%))
         (define p-ctrl (new PositionController% [model m])))
  (check-equal?
    (dynamic-get-field 'x p-ctrl) 150)
  (check-equal?
    (dynamic-get-field 'y p-ctrl) 100)
  (check-equal?
    (dynamic-get-field 'saved-mx p-ctrl) 0)
  (check-equal?
    (dynamic-get-field 'saved-my p-ctrl) 0)
  (check-equal?
    (dynamic-get-field 'selected? p-ctrl) false)
  (send p-ctrl after-button-down 95 80); select the controller
  (check-equal?
    (dynamic-get-field 'selected? p-ctrl) true)
  (check-equal?
    (dynamic-get-field 'text-selected? p-ctrl) false)
  (send p-ctrl after-button-up 150 100); unselect the controller
  (send p-ctrl after-button-down 150 100); select the controller
  (send p-ctrl render-my-image)
  (check-equal?
    (dynamic-get-field 'text-selected? p-ctrl) true)
  (send p-ctrl after-key-event "up")
  (check-equal?
    (particle-x (dynamic-get-field 'p m))
    75)
  (check-equal?
    (particle-y (dynamic-get-field 'p m))
    45)
  (send p-ctrl after-key-event "down")
  (check-equal?
    (particle-x (dynamic-get-field 'p m))
    75)
  (check-equal?
    (particle-y (dynamic-get-field 'p m))
    50)
  (send p-ctrl after-key-event "left")
  (check-equal?
    (particle-x (dynamic-get-field 'p m))
    70)
  (check-equal?
    (particle-y (dynamic-get-field 'p m))
    50)
  (send p-ctrl after-key-event "right")
  (check-equal?
    (particle-x (dynamic-get-field 'p m))
    75)
  (check-equal?
    (particle-y (dynamic-get-field 'p m))
    50)
))
