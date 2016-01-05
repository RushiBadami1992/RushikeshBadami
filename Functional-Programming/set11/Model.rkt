#lang racket

;; the model consists of a particle, bouncing with its center from x=0
;; to x=150.  It accepts commands and reports when its status changes
(require rackunit)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require "constants.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DESIGN

#|
a ListOfControllers<%>(LOC) is a list which is either:
-- empty
-- (cons Controller<%> LOC)

loc-fn: LOC -> ??
(define (loc-fn loc)
    (cond
        [(empty?) ...]
        [else ... (first loc)
              ... (loc-fn (rest loc))]))
|#

;Axis is one of :
; -- "x" interp: along X axis
; -- "y" interp: along Y axis
;; TEMPLATE
;; axis-fn : Axis -> ??
;(define (axis-fn c)
;  (cond
;    [(string=? c "x")    
;     ...]
;    [(string=? c "y")
;     ...])) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide Model%)
;; a Model is a (new Model% [p particle][rect-local rect])

(define Model%
  (class* object% (Model<%>)

    ;; particle use to represent the x and y coordinates of particle and vx and vy to
    ;; represent its velocity.
    (init-field [p particle])
    ;;Use to represent the structure of rectangle.
    (init-field [rect-local rect])
    ;Boolean value which returns if particle is selected or not.
    (field [selected? false])
    (super-new)
    (set! p (make-particle 75 50 0 0))
    (set! rect-local (make-rect 0 150 0 100))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-tick -> Void
    ;; Effect:moves the object by vx and vy.
    ;; if the resulting x is >= 150 or <= 0
    ;; reports particle at ever tick
    ;; DESIGN-STRATEGY:Cases on if it is selected or not
    (define/public (after-tick)
      (if (not selected?)
          (begin
            (set! p (particle-after-tick p rect-local)))
          void))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; keep-in-canvas Particle -> Particle
    ;; GIVEN: a Particle p as input
    ;; RETURNS: a Particle kept in the canvas. If ever the canvas cross the bounds,
    ;;          it will remain on the farthest position withing hte canvas
    ;; DESIGN-STRATEGY: use template of Axis
    (define (keep-in-canvas p)
      (local((define (round-position axis l)
               (cond
                 [(string=? axis "x") (cond
                                        [(> l MAX-WIDTH) MAX-WIDTH]
                                        [(< l MIN-VALUE) MIN-VALUE]
                                        [else l])]
                 [(string=? axis "y") (cond
                                        [(> l MAX-HEIGHT) MAX-HEIGHT]
                                        [(< l MIN-VALUE) MIN-VALUE]
                                        [else l])])))
        (make-particle (round-position "x" (particle-x p))
                       (round-position "y" (particle-y p))
                       (particle-vx p)
                       (particle-vy p))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Command -> Void
    ;; Effect:decodes the command, executes it and update our particle
    ;; DESIGN-STRATEGY:Cases on whether it is particle or not.
    (define/public (execute-command cmd)
      (if (particle? cmd)
        (begin
            (set! p (keep-in-canvas cmd)))
        (set! selected? cmd)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; get-p: -> Particle
 ;; RETURNS:Current state of particle
 ;; Design-Strategy:Combine Simpler Functions.   
    (define/public (get-p) p)
))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (local ((define my-m (new Model%)))
    (send my-m after-tick)
    (check-equal?
     (particle-x (dynamic-get-field 'p my-m)) 75)
    (check-equal?
     (particle-y (dynamic-get-field 'p my-m)) 50)
    (send my-m execute-command (make-particle 160 -50 0 0))
    (check-equal?
     (particle-x (dynamic-get-field 'p my-m)) 150)
    (check-equal?
     (particle-y (dynamic-get-field 'p my-m)) 0)
    (send my-m execute-command (make-particle -10 110 0 0))
    (check-equal?
     (particle-x (dynamic-get-field 'p my-m)) 0)
    (check-equal?
     (particle-y (dynamic-get-field 'p my-m)) MAX-HEIGHT)
    (send my-m execute-command (make-particle 10 10 0 0))
    (check-equal?
     (particle-x (dynamic-get-field 'p my-m)) 10)
    (check-equal?
     (particle-y (dynamic-get-field 'p my-m)) 10)
    (send my-m execute-command true)
    (check-equal?
     (dynamic-get-field 'selected? my-m) true)
    (check-equal?
     (send my-m get-p)
     (make-particle 10 10 0 0))
    ))
