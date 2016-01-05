;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;GOAL:Delete requested character from string character from string
(require rackunit)
(require "extras.rkt")
(provide string-delete)
;;DATA DEFINATIONS:none.
;;string-delete:String PosReal->String.
;;GIVEN:A string and position of character which needs to be deleted from string.
;;WHERE:Entered String should not be empty
;;RETURN:A string after the given character was deleted from string.
;;EXAMPLES:
;;(string-delete "helloworld" 0)="elloworld"
;;(string-delete "helloworld" 5)="hellworld"
;;DESIGN STRATERGY:Combine simpler functions

(define (string-delete word position)
  (cond
    [(= (- 1 position ) 0) (substring word 1 (string-length word))]
    [(= position (string-length word)) (substring word 0 (- (string-length word) 1))]
    [(> position (- (string-length word) 1)) "Postion where you want to delete character does not exist"]
    [else (string-append (substring word 0 (- position  1)) (substring word  position  (string-length word)))]))

;;TESTS:
(begin-for-test
  (check-equal? (string-delete "helloworld" 1) "elloworld"
                "'h' should get deleted from string helloworld")
   (check-equal? (string-delete "helloworld" 5) "hellworld"
                "'o' should get deleted from string helloworld"))