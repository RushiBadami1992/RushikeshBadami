;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal create a fine state machine
(check-location "02" "fsm.rkt")
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(provide initial-state
          next-state
         accepting-state?
         error-state?)

(define S1 "S1")

(define S2 "S2")

(define S3 "S3")
;;DATA DEFINATION:
;;a Finete machine state (FsmState) is one of
;; -- "S1"
;; -- "S2" 
;; -- "S3"
;; -- "error"
;; INTERPRETATION:
;;Where S1 is the first state of machine
;;Where S2 is the second state of machine
;;Where S3 represents the final or acceptance state of machine
;;"error" represents the error state.
;;TEMPLATE
;;fsm-fn:MachineState->??
;;(define (fsm-fn state)
;; (cond
;;   [(string=? state "S1")    ...]
;;   [(string=? state "S2") ...]
;;   [(string=? state "S3")  ...]))  


;;initial-state : Number -> State
;;GIVEN: a number
;;RETURNS: a representation of the initial state of your machine.
;;EXAMPLE:
;;(initial-state 5)="S1"
;;STRATERGY:Combine Simple Functions
(define (initial-state x)
  S1)
;;Test
(begin-for-test
  (check-equal? (initial-state 5) "S1"
          "This function should return the initial state of machine ie S1"))

;;first-state-operations:machine-input->state
;;GIVEN:The machine input given as character
;;RETURN:The change in state of machine after the machine input is entered.
;;EXAMPLE
;;(first-state-operations "a")=S1
;;(first-state-operations "b")=S1
;;(first-state-operations "c")=S2
;;DESIGN STRATERGY:Cases on key events.
(define (first-state-operations machine-input)
  (cond
   [(key=? "a" machine-input) S1]
   [(key=? "b" machine-input) S1]
   [(key=? "c" machine-input) S2]
   [else (error-state? "error")]
   ))
;Test
(begin-for-test
  (check-equal? (first-state-operations "a") "S1"
            "When character a is incountered it must stay in same state is S1")
  (check-equal? (first-state-operations "b") "S1"
          "When character b is incountered it must stay in same state is S1")
  (check-equal? (first-state-operations "c") "S2"
          "When character c is incountered it must go to next state is S2")
(check-equal? (first-state-operations "g") #true
          "Character other than a,b and c should go in error state"))


;;second-state-operations:machine-input->state
;;GIVEN:The machine input given as character
;;RETURN:The change in state of machine after the machine input is entered.
;;EXAMPLE
;;(first-state-operations "a")=S2
;;(first-state-operations "b")=S2
;;(first-state-operations "c")=S3
;;DESIGN STRATERGY:Cases on key events
(define (second-state-operations machine-input)
  (cond
   [(key=? "a" machine-input) S2]
   [(key=? "b" machine-input) S2]
   [(key=? "d" machine-input) S3]
   [else (error-state? "error")]
   ))
;;Test
(begin-for-test
  (check-equal? (second-state-operations "a") "S2"
            "When character a is incountered it must stay in same state is S2")
  (check-equal? (second-state-operations "b") "S2"
          "When character b is incountered it must stay in same state is S2")
  (check-equal? (second-state-operations "d") "S3"
          "When character d is incountered it must go to next state is S3")
  (check-equal? (second-state-operations "c") #true
          "Character other than a,b and d should go in error state"))


;;final-state-operations:machine-input->state
;;GIVEN:The machine input given as character
;;RETURN:The change in state of machine after the machine input is entered.
;;EXAMPLE
;;(final-state-operations "e")=S3
;;(final-state-operations "f")=S3
;;DESIGN STRATERGY:Cases on key events
(define (final-state-operations machine-input)
  (cond
   [(key=? "e" machine-input) S3]
   [(key=? "f" machine-input) S3]
   [else (error-state? "error")]))

 (begin-for-test
  (check-equal? (final-state-operations "e") "S3"
            "When character a is incountered it must stay in same state is S2")
  (check-equal? (final-state-operations "f") "S3"
          "When character b is incountered it must stay in same state is S2")
  (check-equal? (final-state-operations "d") #true
          "Character other than e and f should go in error state"))

;; next-state : Current-State machine-input->next-state
;; GIVEN: a FsmState
;; RETURNS: the FsmState that follows the given FSMState
;; EXAMPLES:
;; (next-state "S1" "a") = "S1"
;; (next-state "S2" "d") = "S3"
;; (next-state "S3" "e") = "S3"
;; STRATEGY: Use template for FsmState on state
(define (next-state current-state machine-input)
    (cond
      [(string=? S1  current-state) (first-state-operations  machine-input)]
      [(string=? S2  current-state)  (second-state-operations machine-input)]
      [(string=? S3  current-state)  (final-state-operations  machine-input)]
     ))
;TEST
(begin-for-test
  (check-equal? (next-state "S1" "a") "S1"
            "When the machine input is a in S1 it should remain in the same state")
  (check-equal? (next-state "S2" "d") "S3"
          "When the machine input is d in S2 it should go to the final state")
(check-equal? (next-state "S3" "e") "S3"
          "When the machine input is e in S3 it should remain in the final state")
  )

;;accepting-state? : State -> Boolean
;;GIVEN: a state of the machine
;;RETURNS: true iff the given state is a final (accepting) state
;;EXAMPLE:
;;(accepting-state "S3")=#True
;;(accepting-state "S2")=#FALSE
;;DESIGN STRATERGY:Use Template.
(define (accepting-state? current-state)
  (string=? S3 current-state))
;Test
(begin-for-test
  (check-equal? (accepting-state? "S3") #T
            "S3 is final state and hence accepting state")
  (check-equal? (accepting-state? "S2") #F
          "As S2 is not accepting state it should return false"))

;;error-state? : State -> Boolean
;;GIVEN: a state of the machine
;;RETURNS: true iff there is no path (empty or non-empty) from the given state to an accepting state
;;DESIGN STRATERGY:Use Template.
(define (error-state? current-state)
  (string=? "error" current-state))
;Test
(begin-for-test
  (check-equal? (error-state? "error") #T
            "S3 is final state and hence accepting state")
  (check-equal? (error-state? "S3") #F
          "As S2 is not accepting state it should return false"))
