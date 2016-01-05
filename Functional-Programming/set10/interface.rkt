#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(provide PlaygroundState<%>
         Toy<%>)
;; INTERFACEs
;; A Playground with an target in it
(define PlaygroundState<%>
  (interface (SWidget<%>)
    
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
  (interface (SWidget<%>)
    
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
