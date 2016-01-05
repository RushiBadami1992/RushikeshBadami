;;Is the super class used to derive all the controllers present in program. 

#lang racket
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "ParticleWorld.rkt")
(require "PerfectBounce.rkt")
(require "Interfaces.rkt")

(provide Controller%)
;; a Controller% is a (new Controller% [model Model<%>][x Int][y Int]
;;                      [selected? boolean][saved-mx Int][saved-my Int][HANDLE-IMG IMG]
;;                      [handle-side Int])

(define Controller%
  (class* object% (SWidget<%>)
    ;the position of the center of the controller
    (init-field [x 0] [y 0])
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    (init-field [saved-mx 0] [saved-my 0])
    (init-field [selected? false])
    ;; Defines the side of handle
    (field [handle-side 10])
    ;; Defines the image of handle
    
    (field [HANDLE-IMG (square handle-side "outline" "black")])
    (init-field [model void])
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-key-event:Key-Event-> Void
    ;; Effect:This function has no effect on Controller
    ;; DESIGN-STRATEGY:Combine Simpler Function    
    (define/public (after-tick) void)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;These methods are implemented in class    
    (abstract after-key-event)
    (abstract render-my-image)
    (abstract controller-selected?)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-to-scene:Scene->Scene
    ;; Given:A scene
    ;; Returns:A image of controller placed in scene
    ;; DESIGN-STRATEGY:Combine Simpler Functions    
    (define/public (add-to-scene s)
      (place-image (render-my-image) x y s))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; after-button-down:NonNegInt NonNegInt -> void
    ;; Effect:Alter the state of selected mx and my coordinates
    ;; EXAMPLE:Check test for example
    ;; DESIGN-STRATEGY:Use cases on if controller is selected or not
    
    (define/public (after-button-down mx my)
      (if (controller-selected? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y))
            )
          void))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-up :NonNegInt NonNegInt->void
    ;; Effect:Alter the state of selected? 
    ;; EXAMPLE:Check Test for example
    ;; DESIGN-STRATEGY:Combine Simpler Functions 
    
    (define/public (after-button-up mx my)
      (set! selected? false))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; after-button-drag:NonNegInt NonNegInt -> void
    ;; Effect:Alter the saved-mx and saved-my coordinates of mouse
    ;; EXAMPLE:Check test for example
    ;; Design-Strategy:use cases on controller is selected or not    
    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          void))
    ))

