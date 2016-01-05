;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rainfall) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;****************************************************************************************
 ;                          PROGRAM DESCRIPTION                 
;****************************************************************************************
;A program that consumes a list of numbers representing daily rainfall amounts .
;The list may contain the number -999 indicating the end of the data of interest.
;Produce the average of the non-negative values in the list up to the first -999
;(if it shows up).There may be negative numbers other than -999 in the list.


(require "extras.rkt")
(require rackunit)
(provide rainfall)
(check-location "07" "rainfall.rkt")
;****************************************************************************************
;; Template for ListOfNumber(LON) is either 
;; --empty
;; --(cons Number LON)

; WHERE Number is a Integer.

;; lon-fn: ListOfNumber->??
;; (define (lon-fn num)
;;  (cond
;;  [(empty? num )...]
;;  [(else
;;          (...(first num)
;;           (lon-fn(rest num)))]))


;****************************************************************************************
;; Constants for Program
     (define ZERO 0)
     (define STOP-CONDITION -999)

;****************************************************************************************

;; rainfall : ListOfNumber -> NonNegReal
;; GIVEN: a list of number which includes integers.
;; RETURNS: the average of the non-negative values in the list up to the first -999 and
;; skipping the negative value.
;; EXAMPLES :- (rainfall (list -2 1 2 3 ))=2
; ;Strategy: Combine Simpler Functions.

(define (rainfall lon)
  (calculate-average-from-list lon ZERO ZERO))
;****************************************************************************************

;; calculate-average-from-list : ListOfNumber NonNegInt NonNegInt -> NonNegReal
;; GIVEN: a list of number which includes integers, sum of the posInt numbers and
;         length of the list of integers.
;; RETURNS: the average of the non-negative values in the list up to the first -999 and
;;          skipping the negative value.
;; WHERE: Sum calculates the sum of all positive numbers and length is 
;; a counter to count the positive numbers in list
;; EXAMPLES :- (calculate-average-from-list (list -2 1 2 3 ))=2
; ;Strategy:Use template for ListOfNumber on lon.


(define (calculate-average-from-list lon sum length)
  (cond
    [(empty? lon)(calculate-average sum length)]
    [else(if (= STOP-CONDITION (first lon))
             (calculate-average sum length)
             (calculate-average-from-list  (rest lon) (calculate-sum (first lon) sum)
                                    (calculate-length (first lon) length)))]))
;***************************************************************************************
;calculate-sum: Integer NonNegInt -> NonNegInt

;GIVEN : An integer and sum .
;RETURN: The sum after adding the postive Integer and not considering the negative
;        numbers.
;EXAMPLE:  (calculate-sum 5 1))6
;           (calculate-sum -5 1))1
;STRATEGY:  Combine simpler functions.

(define (calculate-sum num sum)
  (if (negative? num)
       sum
      (+ sum num)))


;***************************************************************************************

;calculate-length: Integer NonNegReal -> NonNegReal
;GIVEN : An integer and length of the list.
;RETURN: The length after checking if the number is positive or negative
;EXAMPLE:  (calculate-sum 5 1))2
;           (calculate-sum -5 1))1
;STRATEGY:  Combine simpler functions.

(define (calculate-length num len)
  (if (negative? num)
      len
      (+ len 1)))
;****************************************************************************************

;; calculate-average : NonNegInt NonNegInt-> NonNegReal
;; GIVEN: total sum of the numbers in the list after checking the  given conditions on it
;;  and length  of list.
;; RETURNS: the average of the non-negative values in the list up to the first -999
;; encountered and skipping the negative values.
;; EXAMPLE: (calculate-average (list 2 3 4 )))= 3
;; Strategy: Combine Simpler Functions.


(define (calculate-average sum length)
  (if (= sum ZERO)
      ZERO
      (/ sum length)))


;****************************************************************************************
;*****************************************************************************************
 ;TESTS

(begin-for-test
  (check-equal?
   (rainfall (list -2 1 2 3 ))2
    "the avg of the numbers in the list")

  (check-equal?
   (rainfall (list -999 6 5 4 ))0
    "-999 is the first element")
  (check-equal?
   (rainfall (list 0 1 2 -999))1
    "the average should be 1"))

;***************************************************************************************
;                      END OF THE PROGRAM
;***************************************************************************************



  