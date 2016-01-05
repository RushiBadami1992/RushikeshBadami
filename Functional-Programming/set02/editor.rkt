;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;GOAL: To implement a tiny text editor

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(check-location "02" "editor.rkt")
(provide
  make-editor
  editor-pre
  editor-post
  editor?
  edit
  )
;;DATA DEFINATION:
(define LEFT "left")
;;Represents left cursor
(define RIGHT "right")
;;Represents right cursor
(define BACKSPACE "\b")
;;Represents Backspace key
(define RETURN   "\r")
;;Represents Return key
(define TAB "\t")
;;Represents TAB key
(define-struct editor [pre post])
;;A editor is  (make-editor string string)
;;INTERPRETATION
;;pre represents the string before the cursor
;;post represents the string after the cursor

;; TEMPLATE:
;; (define (editor-fn e)
;;   (...
;;     (editor-pre e)
;;     (editor-post  e)))     



;;string-first:string->char
;;GIVEN:A string 
;;RETURN:First character from the string
;;EXAMPLES:
;;(string-first "hello")="h"
;;(string-first "")=""
;;DESIGN STRATERGY: Combine Simpler functions
(define (string-first s)
  (if
   (= 0 (string-length s))
   s
  (string-ith s 0)))
;Test
(begin-for-test
  (check-equal? (string-first "Hello") "H" 
    "should return first character from a string")
   (check-equal? (string-first "") ""
       "should return first character from a string"))

;;string-last:string->char
;;GIVEN:A string 
;;RETURN:Last character of string
;;EXAMPLES:
;;(string-last "Hello")="o"
;;(string-last "")=""
;;DESIGN STRATERGY:Combine Simpler functions
(define (string-last s)
  (if
   (= 0 (string-length s))
   s 
   (string-ith s (- (string-length s) 1))))
;Test
(begin-for-test
  (check-equal? (string-last "Hello") "o" 
    "should return last element from a string")
   (check-equal? (string-last "H") "H"
       "should return last element from a string")
   (check-equal? (string-last "") ""
         "should return nothing"))

;;string-delete-pre:string->string
;;GIVEN:A string whoose last character needs to be deleted.
;;RETURN:A input string with last character removed from it.
;;EXAMPLES:
;;(string-delete-pre "hello")="Hell"
;;DESIGN STRATERGY:Combine Simpler functions
(define (string-delete-pre pre)
  (substring pre 0 (-(string-length pre) 1)))
;;Test
(begin-for-test
  (check-equal? (string-delete-pre "hello") "hell"
  "should delete the last character from given string"))

;;string-append-post:string string->string
;;GIVEN:String pre and post 
;;RETURN:A String with last character of pre appended to post
;;EXAMPLES:
;;(string-append-post "Hello" "World")="oWorld"
;;DESIGN STRATERGY:Combine Simpler Function
 (define (string-append-post pre post)
  (string-append (string-last pre) post))
;;Test
(begin-for-test
  (check-equal? (string-append-post "hello"  "World") "oWorld"
     "should concatinate last letter of pre to post")
  (check-equal? (string-append-post "H" "World") "HWorld"
     "should concatinate last letter of pre to post when cursor moves left")
  (check-equal? (string-append-post ""  "World") "World"
     "should concatinate last letter of pre to post when cursor moves left")
  )

;;left-editor:editor->editor
;;GIVEN:Editor when the cursor has moved to the left
;;RETURN:Editor after performing left cursor operations
;;where last character of pre gets appended to post
;;EXAMPLE:
;;(left-editor (make-editor "hello" "world")=(make-editor "hell" "oworld")
;;STRATERGY:USE TEMPLATE FOR EDITOR ON E
(define (left-editor editor)
  (make-editor (string-delete-pre (editor-pre editor))
               (string-append-post (editor-pre editor) (editor-post editor))))

;;cursor-left:editor->editor
;;GIVEN:Editor when left arrow key is pressed
;;RETURN:Editor after left operation was performed
;;EXAMPLE:
;;(cursorleft (make-editor "hello" "world")=(make-editor "hell" "oworld")
;;DESIGN STRATERGY:Combine Simpler Function
(define (cursorleft editor)
  (left-editor editor))


;;string-append-pre:string string->string
;;GIVEN:The string pre and post  from the editor
;;RETURN:The string consisting of first character of post getting appended to pre of editor
;;EXAMPLE:
;;(string-append-pre "Hello" "World")="HelloW"
;;STRATERGY:Combining Simpler Functions
 (define (string-append-pre pre post)
    (string-append pre (string-first post)))
;Test
(begin-for-test
  (check-equal? (string-append-pre "hello"  "World") "helloW"
     "should concatinate first letter of post to pre when cursor moves right")
  (check-equal? (string-append-pre "Hello" "") "Hello"
     "should concatinate first letter of post to pre when cursor moves right")
  (check-equal? (string-append-pre ""  "World") "W"
     "should concatinate first letter of post to pre when cursor moves right")
  )

;;string-delete-post:string->string
;;GIVEN:String whoose first character needs to be removed
;;RETURN:String after removing first character
;;EXAMPLE:
;;(string-delete-post "Hello")="ello"
;;DESIGN STRATERGY:Combine Simpler Functions
(define (string-delete-post post )
  (if
   (= 0 (string-length post))
    post
  (substring post 1 (string-length post))))
;Test
(begin-for-test
  (check-equal? (string-delete-post "hello" ) "ello"
     "should remove first letter of post when cursor moves right")
  (check-equal? (string-delete-post "") ""
     "should return an empty string as post is empty"))

;;right-editor:editor->editor
;;GIVEN:Editor when the cursor has moved to the right
;;RETURN:Editor after performing right cursor operations where first character of post gets appended to pre
;;EXAMPLE:
;;(right-editor (make-editor "hello" "world")=(make-editor "hellow" "orld")
;;STRATERGY:USE TEMPLATE FOR EDITOR ON E
(define (right-editor e)
  (make-editor (string-append-pre (editor-pre e) (editor-post e))
               (string-delete-post(editor-post e))))

;;cursor-right:editor->editor
;;GIVEN:Editor when right arrow key is pressed
;;RETURN:Editor after right operation was performed
;;EXAMPLE:
;;(cursorright (make-editor "hello" "world")=(make-editor "hellow" "orld")
;;DESIGN STRATERGY:Combine Simpler Functions
(define (cursorright editor)
(right-editor editor
  ))

;;insert-string:string char->string
;;GIVEN:A string and a character which needs to be inserted in string
;;RETURN:String after character has been appended in string
;;EXAMPLE:
;;(insert-string "Hello" "w")="Hellow"
;;DESIGN STRATERGY:Combine Simple function
(define  (insert-string pre ke)
   (string-append pre ke))
;Test
(begin-for-test
  (check-equal? (insert-string "hello"  "W") "helloW"
     "should insert letter after pre"))

;;backspace-delete:string->string
;;GIVEN:A string whoose last character needs to be deleted
;;RETURN:A string after deletion of last character.
;;EXAMPLE:
;;(backspace-delete "Hello")="Hell"
;;DESIGN STRATERGY:Combine Simple Function
(define (backspace-delete pre)
  (if
   (= 0 (string-length pre))
   pre
  (substring pre 0 (- (string-length pre) 1))))
;Test
(begin-for-test
  (check-equal? (backspace-delete "hello") "hell"
     "should delete last letter in pre")
  (check-equal? (backspace-delete "") ""
      "should delete nothing as pre is empty"))

;;backspace:editor-> editor
;;GIVEN:A Editor from which last character of pre needs to be deleted .
;;RETURN:A Editor after deletion of character from pre after backspace operation.
;;EXAMPLE:
;;(backspace (make-editor "hello" "world")=(make-editor "hell" "world")
;;DESIGN STRATERGY:USE TEMPLATE FOR EDITOR ON E
(define (backspace e)
  (make-editor (backspace-delete (editor-pre e))  (editor-post e)))

;;add-string:editor-> editor
;;GIVEN:A Editor where  character needs to be inserted after pre.
;;RETURN:A Editor after character has been inserted after pre.
;;EXAMPLE:
;;(backspace (make-editor "hello" "world")=(make-editor "hell" "world")
;;DESIGN STRATERGY:USE TEMPLATE FOR EDITOR ON E
(define (add-string editor ke)
  (if
   (or (key=? ke RETURN) (key=? ke TAB))
   editor
  (make-editor (insert-string (editor-pre editor) ke)
               (editor-post editor))))
;Test
(begin-for-test
(check-equal? (add-string (make-editor "hello" "world") "\r")
              (make-editor "hello" "world")
                "Editor should do nothing after detecting \r or \t")
(check-equal? (add-string (make-editor "hello" "world") "\t")
              (make-editor "hello" "world")
                "Editor should do nothing after detecting \r or \t")
)
;;edit:editor->editor
;;GIVEN:An Editor on whom the operation needs to be performed
;;RETURN:A Editor after the operation has been performed
;;EXAMPLE
;;(edit (make-editor "Hello" "World" "left")="Hell" "oWorld"
;;(edit (make-editor "Hello" "World" "right")="HelloW" "orld"
(define (edit editor ke)
    (cond
   [(key=? ke LEFT) (cursorleft editor)]
   [(key=? ke RIGHT)(cursorright editor)]
   [(key=? ke BACKSPACE)(backspace editor)]
   [(= (string-length ke)1)(add-string editor ke)]))

(begin-for-test
  (check-equal? (edit (make-editor "hello" "world") "left") (make-editor "hell" "oworld")
     "The editor should perform left key operation")
  (check-equal? (edit (make-editor "hello" "world") "right") (make-editor "hellow" "orld")
      "The editor should perform right key operation")
  (check-equal? (edit (make-editor "hello" "world") "\b") (make-editor "hell" "world")
                "The editor should perform backspace key operation")
  (check-equal? (edit (make-editor "appl" "") "e") (make-editor "apple" "")
               "The editor should append string after pre of editor")       
  )

  




   