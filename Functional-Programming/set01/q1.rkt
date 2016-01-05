;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 13|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;Goal:Calcuate Distance of point from origin

(require rackunit)
(require "extras.rkt")
(provide distance-to-origin)
;;Data Definations:none
;;distance-to-origin:Real->Real
;;GIVEN:Two numbers which represent co-ordinate x and co-ordinate y of a number
;;RETURNS:Distance from origin
;;EXAMPLES:
;;(distance-to-origin 3 4) = 5
;;(distance-to-origin 15 8)=17
;;DESIGN STRATERGY:Combine simpler functions

(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))

;;TESTS
(begin-for-test
  (check-equal? (distance-to-origin 3 4 ) 5
          "Distance  of point x(3,4) from origin should be 5")
  (check-equal? (distance-to-origin 8 15) 17
          "Distance of point x(15,90 from origin should be 17"))