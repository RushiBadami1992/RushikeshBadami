#lang racket


(require 2htdp/image)
(require 2htdp/universe)
;;(require "Interfaces.rkt")
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require "controller.rkt")
(require rackunit)
(require "extras.rkt")
(require "Model.rkt")
(require "constants.rkt")
(provide XController%)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a XController% is a (new XController% [model Model<%>][x Int][y Int]
;;                      [selected? boolean][saved-mx Int][saved-my Int][HANDLE-IMG IMG]
;;                      [handle-side Int])

(define XController%
  ;(class* object% (Controller<%>)
  (class* Controller% ()

    (inherit-field model)  ; the model

    ; the position of the center of the controller
    (inherit-field x)
    (inherit-field y)   

    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    (inherit-field selected?)
    ;; checks if ball is selected or not
    (field [ball-selected? false])
    (inherit-field saved-mx)
    (inherit-field saved-my)
    ;; Defines the image of handle
    (inherit-field HANDLE-IMG)
    ;; Defines the side of handle
    (inherit-field handle-side)
    ;;defines the recttangle in which particle is placed.
    (field [ground (rectangle MAX-WIDTH OFFSET "outline" "blue")])
    ;; defines the outer radius of particle
    (field [radius-out 10])
    ;; defines the inner radius of particle
    (field [radius-in 4])
    ;defines a particle
    (field
     [ball (place-image (circle radius-in "solid" "black") 10 10
                        (circle radius-out "solid" "red"))])

    (super-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (set! x MAX-WIDTH)
    (set! y MAX-HEIGHT)
    (set! saved-my INITIAL-POINT)
    (set! saved-mx INITIAL-POINT)
    (set! selected? false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; controller-selected? NonNegInt NonNegInt -> Boolean
;; Given:The x and y coordinates of mouse
;; RETURNS:If the handle is selected or not
;; EXAMPLE:Check test for example.
;; DESIGN-STRATEGY:Combine Simpler Functions     
    (define/override (controller-selected? other-x other-y)
      (local(
             (define rectangle-width (+ (image-width ground) OFFSET))
             (define rectangle-height (image-height ground))
             (define handle-x ( + x (- (/ handle-side HALF) (/ rectangle-width HALF))))
             (define handle-y ( + y (- (/ handle-side HALF) (/ rectangle-height HALF))))
             )
      (and
        (<= (abs (- other-x handle-x))  (/ handle-side HALF))
        (<= (abs (- other-y handle-y))  (/ handle-side HALF)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; after-key-event:Key-Event-> Void
;; Effect:This function has no effect on XController
;; DESIGN-STRATEGY:Combine Simpler Function    
    
    (define/override (after-key-event key) void)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; current-color:Boolean-> String
;; GIVEN:A true or false to retun color to be rendered for text
;; RETURNS:A color of string accordingly
;; EXAMPLE:Check Test for Example
;; DESIGN-STRATEGY:Combine Simpler Functions.    
      
    (define (current-color predicate)
      (if predicate RED BLACK))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-down:NonNegInt NonNegInt -> void
;; Effect:Alter the state of selected and text selected.
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Use cases on if ball is selected or not
    
    (define/override (after-button-down mx my)
      (local(
            (define my-p (send model get-p))
            (define (is-ball-selected?)
              (and
                (<= (abs (- mx x)) (/ MAX-WIDTH HALF))
                (<= (abs (- my y)) (/ OFFSET HALF)))))
      (begin 
        (super after-button-down mx my)
        (if (is-ball-selected?)
            (begin
              (set! ball-selected? true)
              (send model execute-command true); set model selected
              (set! saved-mx mx)
              (set! saved-my my))
            void)
      )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
;; after-button-drag:NonNegInt NonNegInt -> void
;; Effect:Alter the saved-mx and saved-my coordinates of mouse
;; EXAMPLE:Check test for example
;; Design-Strategy:use cases on ball selected or not    
    (define/override (after-drag mx my)
      (local((define my-p (send model get-p)))
      (begin
      (super after-drag mx my)
      (if ball-selected?
       (begin
         (send model execute-command 
           (make-particle (+ (particle-x my-p)(- mx saved-mx))
                          (particle-y my-p) (particle-vx my-p) (particle-vy my-p)))
         (set! saved-mx mx)
         (set! saved-my my))
        void))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; after-button-up :NonNegInt NonNegInt->void
;; Effect:Alter the state of selected? and text-selected?
;; EXAMPLE:Check Test for example
;; DESIGN-STRATEGY:Combine Simpler Functions 
    (define/override (after-button-up mx my)
      (begin
        (super after-button-up mx my)
        (set! ball-selected? false)
        (send model execute-command false))); set model selected

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; render-image -> Image
;; RETURN:An image of position controller
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Combine Simpler Functions    
    
    (define/override (render-my-image)
      (local ((define my-p (send model get-p)))
        (overlay/xy
             (overlay/xy
                ground
                INITIAL-POINT
                INITIAL-POINT
             (place-image
                ball
                (+ (particle-x my-p) INITIAL-POINT)
                HALF-OFFSET-POS
                ground))
            HALF-OFFSET
            INITIAL-POINT
            (overlay/xy
                (square handle-side OUTLINE (current-color selected?))
                
                INITIAL-POINT
                INITIAL-POINT
              (rectangle
                (+ (image-width ground) OFFSET)
                (image-height ground)
                OUTLINE 
                BLACK)))))


    ))

(begin-for-test
  (local((define m (new Model%))
         (define p-ctrl (new XController% [model m])))
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
  (send p-ctrl after-button-down 60 85); select the controller
  (send p-ctrl render-my-image)
  (check-equal?
    (dynamic-get-field 'selected? p-ctrl) true)
  (check-equal?
    (dynamic-get-field 'ball-selected? p-ctrl) false)
  (send p-ctrl after-button-up 150 100); unselect the controller
  (send p-ctrl after-button-down 150 100); select the controller
  (send p-ctrl render-my-image)
  (send p-ctrl after-drag 140 100); move by -10 along x axis
  (check-equal? (particle-x (send m get-p)) 65)
))
