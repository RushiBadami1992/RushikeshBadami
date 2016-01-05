#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define OFFSET 40)
(define HALF 2)
(define MAX-WIDTH 150)
(define MAX-HEIGHT 100)
(define HALF-OFFSET -20)
(define INITIAL-POINT 0)
(define TEXT-SIZE 10)
(define INITIAL-POSITION 0)
(define INCREASE-VELOCITY 5)
(define MIN-VALUE 0)
(define HALF-OFFSET-POS 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;COLOR CONSTANTS
(define RED "red")
(define BLACK "black")
(define OUTLINE "outline")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KeyEvents
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide MAX-WIDTH
         OFFSET
         HALF 
         MAX-WIDTH 
         MAX-HEIGHT 
         HALF-OFFSET 
         INITIAL-POINT 
         TEXT-SIZE 
         INITIAL-POSITION 
         INCREASE-VELOCITY 
         MIN-VALUE 
         RED 
         BLACK 
         OUTLINE 
         LEFT 
         RIGHT
         UP 
         DOWN
         HALF-OFFSET-POS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
