#lang racket
;;This program is used to implement MVC design pattern in Racket
(require rackunit)
(require "extras.rkt")
(require "Model.rkt")
(require "ParticleWorld.rkt")
(require "ControllerFactory.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;run : PosReal -> Void
;;GIVEN: a frame rate, in sec/tick
;;EFFECT: Creates and runs the MVC simulation with the given frame rate
(define (run rate)
  (let* ((m (new Model%))
         (w (make-world m 600 500)))
    (begin
      (send w add-widget
        (new ControllerFactory% [m m][w w]))
      (send w run rate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
