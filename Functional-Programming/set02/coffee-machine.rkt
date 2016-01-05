;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;GOAL:To implement a simulated coffee machine
(check-location "02" "coffee-machine.rkt")
(require rackunit)
(require "extras.rkt")
(provide
  initial-machine
  machine-next-state
  machine-output
  machine-remaining-coffee
  machine-remaining-chocolate
  machine-bank)
;;DATA DEFINATION:
(define-struct machinestate(numberofcoffee numberofchocolate change bank))
;;A Machine State is (make-machinestate PosNum PosNum PosNum PosNum)
;;INTERPRETATION:
;;numberofcoffee represents the cups of coffee in machine
;;numberofchocolate represents the cups of hot chocolate in machine
;;change represents the change to be returned to customer
;;bank represents the money deposited by machine in bank
;;Template
;; (define (machinestate-fn machinestate)
;;   (...
;;     (machinestate-numberofcoffee machinestate)
;;     (machinestate-numberofchocolate  machinestate)
;;     (machinestate-change machinestate)
;;     (machinestate    machinestate))     

;;A customerInput is one of
;;--a PosInt
;;--"coffee"
;;--"hot chocolate"
;;--"change"
;;Interpretation
;;Money deposited by  customer
;;Coffee asked by customer
;;hot chocolate asked by customer
;;the left over money requested by customer after transaction.

;;TEMPLATE
;;customerInput-fn:customerInput->??
;;(define (customerInput-fn customerInput)
;; (cond
;;   [(string=? customerInput "coffee") ...]
;;   [(string=? customerInput "hot chocolate") ...]
;;   [(string=? customerInput "change")  ...]
;;   [(string=? customerInput "PosNum")  ...]
;;   ))  

;;A MachineOutput is one of
;;coffee
;;hot chocolate
;;Out of item
;;a PosInt
;;TEMPLATE
;;machineinput-fn:machineinput->??
;;(define (machineinput-fn machineinput)
;;(cond
;;   [(string=? customerInput "coffee") ...]
;;   [(string=? customerInput "hot chocolate") ...]
;;   [(string=? customerInput "PosNum")  ...]
;;   ))  

;;reduceitem:Posnum->Posnum
;;GIVEN:The number of cups of coffee or hot chocolate
;;RETURN:Reduces the cup of coffee or hot chocolate by 1
;;EXAMPLE:
;;(define (reduceitem  10)=9
;;DESIGN STRATERGY:Combine Simpler Functions
(define (reduceitem itemquantity)
  (- itemquantity 1))
;Test
(begin-for-test
  (check-equal? (reduceitem 10) 9
       "The quantity of coffee or hotchocolate should get reduced by one "))

;;reducechange:PosNum PosNum->PosNum
;;GIVEN:The change and the amount that is to be reduced from change
;;RETURN:The change after reducing amount
;;EXAMPLE:
;;(reducechange 200 150)=50
;;DESIGN STRATERGY:Combine Simpler Function.
(define (reducechange change amount)
  (- change amount))
;Test
(begin-for-test
  (check-equal? (reducechange 150 150) 0
       "The change should get reduced by 150 cents when coffee is selected")
  (check-equal? (reducechange 150 60) 90
       "The change should get reduced by 150 cents when coffee is selected")
  )
;;addmoney:PosNum PosNum->PosNum
;;GIVEN:The bank and amount to be added to bank
;;RETURNS: The money in bank after the amount has got added
;;EXAMPLE:
;;(addmoney 300 150)=450
;;DESIGN STRATERGY:Combine Simpler Function.
(define (addmoney bank amount)
  (+ bank amount))
;Test
(begin-for-test
  (check-equal? (addmoney 150 150) 300
       "The change should get reduced by 150 cents when coffee is selected" ))

;;coffeeoperation:machinestate->machinestate
;;GIVEN:Machinestate
;;RETURN:Machinestate after dispensing coffee
;;EXAMPLE
;;(coffeeoperation (make-machinestate 12 10 200 100))=(make-machinestate 11 10 50 250)
;;DESIGN STRATERGY:Use template on machinestate machinestate
(define (coffeeoperation machinestate)
  (if
    (and (> (machinestate-change machinestate) 150)
         (> (machinestate-numberofcoffee machinestate) 0))
    (make-machinestate
     (reduceitem (machinestate-numberofcoffee machinestate))
     (machinestate-numberofchocolate machinestate)
     (reducechange (machinestate-change machinestate) 150)
     (addmoney (machinestate-bank machinestate) 150)) 
     machinestate  
     ))
;;Test
(begin-for-test
  (check-equal? (coffeeoperation (make-machinestate 10 9 200 0))
                (make-machinestate 9 9 50 150)
       "It should perform coffee machine")
 (check-equal? (coffeeoperation (make-machinestate 0 9 200 200))
               (make-machinestate 0 9 200 200)
       "As machine is out of coffee it should return nothing"))

;;chocolateoperation:machinestate->machinestate
;;GIVEN:Machinestate
;;RETURN:Machinestate after dispensing hot chocolate
;;EXAMPLE
;;(chocolateoperation (make-machinestate 12 10 200 100))=(make-machinestate 12 9 140 60)
;;DESIGN STRATERGY:Use template on machinestate machinestate
(define (chocolateoperation machinestate)
  (if
    (and (> (machinestate-change machinestate) 60) (> (machinestate-numberofchocolate machinestate) 0))
    (make-machinestate (machinestate-numberofcoffee machinestate)
                       (reduceitem (machinestate-numberofchocolate machinestate))
                       (reducechange (machinestate-change machinestate ) 60)
                       (addmoney(machinestate-bank machinestate) 60))
    machinestate
    ))

;Test
(begin-for-test
  (check-equal? (chocolateoperation(make-machinestate 10 9 200 0)) (make-machinestate 10 8 140 60)
       "It should perform hot chocolate operation")
 (check-equal? (chocolateoperation(make-machinestate 0 0 200 200)) (make-machinestate 0 0 200 200)
       "As machine is out of hot chocolate it should return nothing"))

;;changeoperation:machinestate->machinestate
;;GIVEN:Machinestate
;;RETURN:Machinestate after dispensing change
;;EXAMPLE
;;(changeoperation(make-machinestate 12 10 200 100))=(make-machinestate 12 9 0 100)
;;DESIGN STRATERGY:Use template on machinestate machinestate
(define (changeoperation machinestate)
  (make-machinestate (machinestate-numberofcoffee machinestate)
                     (machinestate-numberofchocolate machinestate)
                     0
                     (machinestate-bank machinestate)))
;TEST:
(begin-for-test
  (check-equal? (changeoperation (make-machinestate 10 9 200 0)) (make-machinestate 10 9 0 0)
       "It should return change in machine"))

;;addinchange:machinestate customerinput -> machinestate
;;GIVEN:Machinestate and customerinput
;;RETURN:State of machine after accepting money from customer
;;EXAMPLE:
;;(addinchange (make-machinestate 10 15 200 200) 200) = (make-machinestate 10 15 400 200)
;;DESIGN STRATERGY:Use template on machinestate machinestate
(define  (addinchange machinestate customerinput)
    (make-machinestate (machinestate-numberofcoffee machinestate)
                     (machinestate-numberofchocolate machinestate)
                      (addmoney (machinestate-change machinestate) customerinput) 
                     (machinestate-bank machinestate))) 
;Test
  (begin-for-test
  (check-equal? (addinchange(make-machinestate 10 9 200 0) 200)
                (make-machinestate 10 9 400 0)
       "It should add money entered by user"))

;;machine-next-state : MachineState CustomerInput -> MachineState
;;GIVEN: a machine state and a customer input
;;RETURNS: the state of the machine that should follow the customer's input
;;EXAMPLE:
;;(define machine-next-state (make-machinestate 10 12 200 400) "coffee")=(make-machinestate 9 12 50 550)
;;DESIGN STRATERGY:Use cases on customer input.
(define (machine-next-state machinestate customerinput)
  (cond
    [(integer? customerinput )(addinchange machinestate customerinput)]
    [(string=? customerinput "coffee")(coffeeoperation machinestate)]
    [(string=? customerinput "hot chocolate")(chocolateoperation machinestate)]
    [(string=? customerinput "change")(changeoperation machinestate)]
  ))
;TEST:
(begin-for-test
  (check-equal? (machine-next-state (make-machinestate 10 9 200 0) "coffee")
                (make-machinestate 9 9 50 150)
       "Machine should dispense coffee from machine")
  (check-equal? (machine-next-state (make-machinestate 10 9 200 150) "hot chocolate")
                (make-machinestate 10 8 140 210)
       "Machine should dispense hot chocolate from  machine")
  (check-equal? (machine-next-state (make-machinestate 10 9 200 150) "change")
                (make-machinestate 10 9 0 150)
       "Machine should dispense change from  machine")
  (check-equal? (machine-next-state (make-machinestate 10 9 200 150) 150)
                (make-machinestate 10 9 350 150)
       "Machine  should add money given by customer to machine"))


;;change-output-coffee:PosNum->string
;;GIVEN:Change as positive number to check whether change is greater than 150 cents
;;RETURN:Coffee if customer has entered enough money in machine else nothing
;;EXAMPLE:
;;(change-output-coffee 200)=coffee
;;DESIGN STRATERGY:Combine Simpler Functions
(define (change-output-coffee change)
  (if
   (> change 150)
   "coffee"
   "Nothing"
   ))

;;returncoffee:PosNum PosNum->string
;;GIVEN:Change as positive number and numberofcoffee as positive number
;;RETURN:Coffee if customer has entered enough money in
;;machine or numberofcoffee is greater than 0 else nothing
;;EXAMPLE:
;;(returncoffee 20 200)=coffee
;;DESIGN STRATERGY:Combine Simpler Functions
(define (returncoffee numberofcoffee change)
  (if
   (> numberofcoffee  0)
      (change-output-coffee change)
      "Out of item"
      ))

;Test
(begin-for-test
  (check-equal? (returncoffee 0 200) "Out of item"
                "When the machine has 0 cups left it should return out of coffee")
(check-equal? (returncoffee 5 200) "coffee"
                "The machine should return coffee when the change
                 is more than 150 cents and cups more than 0")
(check-equal? (returncoffee 5 100) "Nothing"
                "Machine should return nothing when change is less than priceofcoffee"))


;;change-output-chocolate:PosNum->string
;;GIVEN:Change as positive number 
;;RETURN:hot chocolate if customer has entered enough money in machine
;;EXAMPLE:
;;(returncoffee 20 200)=hot chocolate
;;DESIGN STRATERGY:Combine Simpler Functions
(define (change-output-chocolate change)
  (if
   (> change 60)
   "hot chocolate"
   "Nothing"
   ))


;;returncoffee:PosNum PosNum->string
;;GIVEN:Change as positive number and numberofhotchocolate as positive number
;;RETURN:hot chocolate if customer has entered enough money in machine or numberofhotchocolate is greater than 0 else nothing
;;EXAMPLE:
;;(returncoffee 20 200)=hot chocolate
;;DESIGN STRATERGY:Combine Simpler Functions
(define (returnchocolate numberofchocolate change)
  (if
   (> numberofchocolate  0)
      (change-output-chocolate change)
      "Out of item"
      ))
(begin-for-test
  (check-equal? (returnchocolate 0 200) "Out of item"
                "When the machine has 0 cups left it should return out of coffee")
(check-equal? (returnchocolate 5 200) "hot chocolate"
                "The machine should return hot chocolate when the change is more than 60 cents and cups more than 0")
(check-equal? (returnchocolate 5 50) "Nothing"
                "Machine should return nothing when change is less than priceofhotchocolate"))


;;machine-next-state : MachineState CustomerInput -> MachineState
;;GIVEN: a machine state and a customer input
;;RETURNS: the output of the machine on user input
;;EXAMPLE:
;;(define machine-output (make-machinestate 10 12 200 400) "coffee")=coffee
;;DESIGN STRATERGY:Use cases on customer input.
(define (machine-output machinestate customerinput)
(cond
  [(string=? customerinput "coffee")
   (returncoffee (machinestate-numberofcoffee machinestate)(machinestate-change machinestate))]
  [(string=? customerinput "hot chocolate")
   (returnchocolate (machinestate-numberofchocolate machinestate)
                    (machinestate-change machinestate))]
  [(string=? customerinput "change") (machinestate-change machinestate)]
  ))

(begin-for-test
  (check-equal? (machine-output (make-machinestate 10 20 200 400) "coffee" ) "coffee"
                "The machine output should be coffee")
(check-equal? (machine-output
               (make-machinestate 10 20 200 400) "hot chocolate" ) "hot chocolate"
                "The machine output should be hot chocolate")
(check-equal? (machine-output (make-machinestate 10 20 200 400) "change" ) 200
                "The machine output should be the change of customer"))

;;initial-machine : NonNegInt NonNegInt -> MachineState
;;GIVEN: a number of cups of coffee and of hot chocolate
;;RETURNS: the state of a machine loaded with the given number of cups
;;         of coffee and of hot chocolate, with an empty bank.
;;Example
;;(intial-machine 20 30)=(make-machinestate 20 30 0 0)
;;DESIGN STRATERGY:Combine Simpler Function
(define (initial-machine noofcoffee noofchocolate )
 (make-machinestate noofcoffee noofchocolate 0 0))
;Test
(begin-for-test
  (check-equal? (initial-machine 10 15) (make-machinestate 10 15 0 0)
                "Values of coffee and hot-chocolate should be set to machine state"))

;;machine-remaining-coffee : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the number of cups of coffee left in the machine
;;EXAMPLE:
;;(machine-remaining-coffee (make-machinestate 10 10 200 300)=10
;;STRATERGY :Combine Simpler Function                          
(define (machine-remaining-coffee machinestate)
(machinestate-numberofcoffee machinestate)) 
;Test
(begin-for-test
  (check-equal? (machine-remaining-coffee (make-machinestate 10 15 0 0)) 10
                "Value of remaining coffee in the machine"))


;;machine-remaining-chocolate : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the number of cups of hotchocolate left in the machine
;;EXAMPLE:
;;(machine-remaining-chocolate (make-machinestate 10 10 200 300)=10
;;STRATERGY :Combine Simpler Function                          
(define (machine-remaining-chocolate machinestate)
  (machinestate-numberofchocolate machinestate))
;Test
(begin-for-test
  (check-equal? (machine-remaining-chocolate (make-machinestate 10 15 0 0)) 15
                "Value of remaining chocolate in the machine"))
;;machine-bank : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS:  the amount of money in the machine's bank, in cents
;;EXAMPLE:
;;(machine-bank (make-machinestate 10 10 200 300)=300
;;STRATERGY :Combine Simpler Function                          
(define (machine-bank machinestate)
      (machinestate-bank machinestate))
;Test
  (begin-for-test
  (check-equal? (machine-bank (make-machinestate 10 15 5 25)) 25
                "Value of remaining amount in the bank"))