;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Goal:To print first character from given string
(require rackunit)
(require "extras.rkt")
(provide string-first)
;;DATA DEFINATIONS:None
;;string-first:string->string
;;GIVEN : A string of characters
;;RETURN: First character of String of characters
;;EXAMPLES:
;;(string-first "Rushikesh")= "R"
;;(string-first "Badami")="B"

;;DESIGN STRATERGY: Combine Simpler Functions

(define (string-first word)
 (string-ith word 0))
;;TESTS:
(begin-for-test
  (check-equal? (string-first "Rushikesh") "R"
                "First letter of string Rushikesh will be R")
  (check-equal? (string-first "Badami") "B"
                "First letter of string Badami will be B"))