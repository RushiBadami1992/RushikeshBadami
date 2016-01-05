;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Goal:Calculate area of image in pixel

(require rackunit)
(require "extras.rkt")
(provide image-area)
(require 2htdp/image)

;;DATA DEFINATIONS:none
;;image-area:image->Posreal
;;GIVEN:Image whose area need to be calculated
;;RETURN:Area of image of pixel
;;EXAMPLES:
;;(image-area (bitmap "cat.jpg"))=1069056
;;(image-area (circle 30 "solid" "orange"))=3600
;;DESIGN STRATERGY:Combine simpler functions

(define (image-area image)
  (* (image-height image) (image-width image))
  )
;;TESTS
(begin-for-test
  (check-equal? (image-area (bitmap "cat.jpg")) 1069056
                    "Area of image cat is 1069056" )
  (check-equal? (image-area (circle 30 "solid" "orange")) 3600 
                    "Area of image circle is 3600" ))
