#lang racket

(require "Interfaces.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "XYController.rkt")
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(require "Model.rkt"); for test
(require "ParticleWorld.rkt"); for test
(provide ControllerFactory%)


;; a ControllerFactory is a (new ControllerFactory% [m Model<%>][w World<%>])
(define ControllerFactory%
  (class* object% (SWidget<%>)

    ; the world in which the controllers will live
    (init-field w)   ; World<%>

    ; the model to which the controllers will be connected
    (init-field m)   ; Model<%>

    (super-new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;after-key-event: KeyEvent -> Void
    ;; Effect:Adds a new controller to the ListOfControllers
    ;; EXAMPLE:Check test for example
    ;; DESIGN-STRATEGY:Use cases on key events.
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "v") (add-viewer VelocityController%)]
        [(key=? kev "p") (add-viewer PositionController%)]
        [(key=? kev "x") (add-viewer XController%)]
        [(key=? kev "y") (add-viewer YController%)]
        [(key=? kev "z") (add-viewer XYController%)]
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; add-viewer-class: ->void
 ;; Effect:Adds a new widget or controller to world
 ;; DESIGN-STRATEGY:Combine Simpler Functions.  
    (define (add-viewer viewer-class)
      (send w add-widget (new viewer-class [model m])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; add-to-scene: Scene-> Scene
 ;; Returns: adds and returns a scene
   (define/public (add-to-scene s) s)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;  after-tick:->Void
 ;;  Effect:It has no effect on ControllerFactory
   (define/public (after-tick) void)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;   after-button-down:PosInt PosInt->void
 ;;   Effect:It has no effect on ControllerFactory

    (define/public (after-button-down mx my)
      void)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
 ;;   after-drag:PosInt PosInt->void
 ;;   Effect:It has no effect on ControllerFactory
    
    (define/public (after-drag mx my)
      void)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
 ;;   after-button-up:PosInt PosInt->void
 ;;   Effect:It has no effect on ControllerFactory
    
    (define/public (after-button-up mx my)
      void)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (local((define m (new Model%))
         (define w (make-world m 600 500))
         (define my-f (new ControllerFactory% [m m][w w])))
  (send my-f after-key-event "v")
  (check-equal? (length (dynamic-get-field 'widgets w)) 1)
  (send my-f after-key-event "p")
  (check-equal? (length (dynamic-get-field 'widgets w)) 2)
  (send my-f after-key-event "x")
  (check-equal? (length (dynamic-get-field 'widgets w)) 3)
  (send my-f after-key-event "y")
  (check-equal? (length (dynamic-get-field 'widgets w)) 4)
  (send my-f after-key-event "z")
  (check-equal? (length (dynamic-get-field 'widgets w)) 5)
  (check-equal? (send my-f add-to-scene void)
                void)
))
  
