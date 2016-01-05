;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;GOAL:Insert a character between string
(require rackunit)
(require "extras.rkt")
(provide string-insert)
;;DATADEFINATIONS :none
;;string-insert:string real -> string
;;Given: A string and position where you have to insert the character "_"
;;WHERE:Entered String is not empty.
;;Return:A string with the character "_" inserted at the given position
;;EXAMPLES
;;(string-insert "helloworld" 0) ="_helloworld"
;;(string-insert "helloworld" 5) ="hello_world"
;;(string-insert "helloworld" 10)="helloworld_"
;;DESIGN STRATERGY:Combine simpler functions
 
(define (string-insert word position)
  (cond
   [(= (- 1 position) 0) (string-append "_" word)]
     [(= position (string-length word)) (string-append word "_")]
     [(> position (- (string-length word) 1)) "Postion where you want to insert character is greater than string length"]
     [else (string-append(substring word 0 (- position 1)) "_" (substring word (- position 1) (string-length word )))])
    )
;;TESTS
(begin-for-test
  (check-equal? (string-insert "helloworld" 5) "hell_oworld"
                "character '_'should get inserted between  hello and world as position to insert string is 5")
   (check-equal? (string-insert "helloworld" 1) "_helloworld"
                 "character '_' should get inserted before helloworld as position to insert is 0")
   (check-equal? (string-insert "helloworld" 10) "helloworld_"
                 "character '_' should get inserted after helloworld"))
