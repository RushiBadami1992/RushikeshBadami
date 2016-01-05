;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rosters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;GOAL:To implement the roster problem using higher order functions.To check if a
;;duplicate roster exist .Create a set of roster from  set of enrollment and return
;;the list of rosters to registrar. 

(require rackunit)
(require "extras.rkt")
(check-location "05" "rosters.rkt")
(provide
make-enrollment
enrollment-student
enrollment-class
make-roster
roster-classname
roster-students
roster=?
rosterset=?
enrollments-to-rosters)
 
(define-struct  enrollment(student class))
;;Enrollment is (make-enrollment Student Class)
;;Interpretation
;;Student is name of Student
;;Class is the class a student studies in

;;Template
;; enrollment-fn: Enrollment->??
;; (define (enrollment-fn enrollment)
;;  (...
;;  (enrollment-student enrollment)
;;  (enrollment-class enrollment))     

(define-struct roster(classname students))
;; Roster is (make-roster Classname Students)
;; Interpretation
;; Classname is the subject where student studies
;; Students is a set of students enrolled in particular class
;; Template:
;; roster-fn: Roster->??
;; (define (roster-fn roster)
;;  (...
;;  (roster-classname roster)
;;  (roster-students roster))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Set of Rosters (SOR) is list of class rosters which is either
;;--empty
;;--(cons Roster SOR)
;; sor-fn: SOR -> ??
;; (define (sor-fn sor)
;;  (cond
;;   [(empty? sor)...]
;;   [else (...
;;          (roster-fn(first sor))
;;          (sor-fn(rest sor)))]))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set of Enrollments (SOE) is list of enrollments is either
;;--empty
;;--(cons Enrollment SOE)
;; soe-fn:SOE->??
;; (define (soe-fn soe)
;;   (cond
;;    [(empty? soe)....]
;;    [else (...
;;           (enrollment-fn(first soe))
;;           (soe-fn(rest soe)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A SetOfX is a ListOfX with no duplicates
;; A List of student is ListOfX
;; A ListOfX is either
;; -- empty
;; -- (cons X ListOfX)

;; TEMPLATE:
;; lox-fn : ListOfX -> ??
;; (define (lox-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... 
;;             (first lst)
;;             (lox-fn (rest lst)))]))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Constants
(define enrollment1 (make-enrollment "John" "PDP"))
(define enrollment2 (make-enrollment "Kathryn" "Networks"))
(define enrollment3 (make-enrollment "Feng" "PDP"))
(define enrollment4 (make-enrollment "Amy" "PDP"))
(define enrollment5 (make-enrollment "Amy" "Networks"))
(define roster1(make-roster "PDP" (list "John" "Feng" "Amy")))
(define roster2(make-roster "Networks" (list "Kathryn" "Amy")))
(define roster3(make-roster "PDP" (list  "Amy"  "Feng" "John")))
(define roster4(make-roster "Networks" (list "Feng" "Amy")))
(define roster5(make-roster "PDP" (list "John" "Feng" "Amy")))
(define roster6(make-roster "Networks" (list "Kathryn" "Amy")))
(define roster7(make-roster "Networks" (list "Kathryn" "Amy")))
(define roster8(make-roster "Networks" (list "Kathryn" "Amy")))
(define roster9(make-roster "PDP" (list "Kathryn" "Amy")))
(define roster10(make-roster "Networks" (list "Kathryn" "Amy")))
(define roster11(make-roster "Networks" (list "Kathryn" "Samy")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; roster?:Roster Roster -> Boolean
;; GIVEN:Two rosters
;; RETURNS:True iff the given two rosters are equal and false if they are not equal
;; EXAMPLE:Check tests for example
;; DESIGN-STRATERGY:Use template of roster on classroster1 and classroster2.
(define (roster=? classroster1 classroster2)
  (and (equal? (roster-classname classroster1) (roster-classname classroster2))
       (student-list-equal? (roster-students classroster1)
                            (roster-students classroster2))))

;;TEST
(begin-for-test
  (check equal? (roster=? roster6 roster7)#t
                            "Should return true as both rosters are same")
  (check equal? (roster=? roster8 roster9)#f
                            "Should return true as both rosters are same")
  (check equal? (roster=? roster10 roster11)#f
                           "Should return true as both rosters are same"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; student-list-equal? SetOfX SetOfX -> Boolean
;; GIVEN: Two SET of X to check if they are true or not
;; RETURNS: true iff both sets are equal else false.
;; EXAMPLE: Check Test for example
;; DESIGN-STRATERGY:Combine Simpler Function

(define (student-list-equal? set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; member-of-set?:X SetOfX -> Boolean
;; GIVEN:A X and a SetOfX
;; RETURNS:true iff that student is present in SOS
;; EXAMPLE:Check Test for example
;; DESIGN-STRATERGY:Use HOF ormap on sets 

(define (member-of-set? x  sets)
  (ormap
   ;;X->Boolean
   (lambda (set) (equal? x  set))
   sets))
;;Test
(begin-for-test
  (check-true (member-of-set? 3 (list 1 3 5)))
  (check-false (member-of-set? 4 (list 1 3 5))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subset?: SetOfX SetOfX -> Boolean
;; GIVEN:TWO SetOfX to check if one is subset of another 
;; RETURNS:true iff one set is subset of another else false
;; EXAMPLE: Check Test for Example
;; DESIGN-STRATERGY:Use HOF andmap on set1
(define (subset? set1 set2)
  (andmap
   ;;X->Boolean
   (lambda (elt) (member-of-set? elt set2))
   set1))
;;TEST
(begin-for-test
  (check-true (subset? (list 1 3 5) (list 1 3 2 4 5 8)))
  (check-false (subset? (list 1 3 5) (list 1 3 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rosterset=?:SOR SOR -> Boolean
;; GIVEN:Two sets of roster to check if duplicate exists in them or not
;; RETURNS:True iff duplicate exists or else false
;; EXAMPLES:Check test for example
;; DESIGN-STRATERGY:Use HOF andmap on sor1

(define (rosterset=? sor1 sor2)
  (andmap
   ;;Roster->Boolean
   (lambda (roster1)(check-for-individual-roster? roster1 sor2))
   sor1))

;;Test
(begin-for-test
(check equal?(rosterset=? (list roster3 roster4)
             (list roster5 roster6))#f
              "Should return false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-for-individual-roster?: Roster SOR-> Boolean
;; GIVEN:A Roster and a set of rosters to check if roster exists in that set.
;; RETURNS:true iff roster exists in list else false
;; EXAMPLE:Check test of rosterset=? for example
;; DESIGN-STRATERGY:Use HOF ormap on sor2

(define (check-for-individual-roster? roster1 sor2)
  (ormap
   ;;Roster->Boolean
   (lambda (roster2)(roster=? roster1 roster2))
   sor2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enrollments-to-rosters: SOE -> LOR
;; GIVEN:A set of enrollments
;; RETURNS: A set of Rosters
;; EXAMPLE:Check test for example
;; DESIGN STRATERGY:Use HOF foldr on enrollments

(define (enrollments-to-rosters enrollments)
  (foldr
   add-to-roster 
   empty
   enrollments))

;;TEST
(begin-for-test
  (check equal?
  (enrollments-to-rosters
    (list enrollment1 enrollment2
          enrollment3 enrollment4 enrollment5))(list roster1 roster2)
                                               "Should return list of rosters"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-to-roster: Enrollment -> SOR
;; GIVEN: A enrollments
;; RETURNS:Adds the enrollment and returns a set of rosters
;; EXAMPLE:Check Test of enrollments-to-rosters for example
;; DESIGN-STRATERGY:Use template of enrollment on enrollment

(define (add-to-roster enrollment rosters)
  (if (check-roster-for-subject? (enrollment-class enrollment) rosters)
      (add-student-name-to-class enrollment rosters)
      (cons(make-roster (enrollment-class enrollment)
                        (list (enrollment-student enrollment)))rosters))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-roster-for-subject? Enrollment SOR -> boolean
;; GIVEN:A enrollment and a set of rosters to check if class exists in set of rosters
;; RETURNS:true if class is present in set of rosters else false
;; EXAMPLE:Check test for example
;; DESIGN-STRATERGY:Use HOF ormap on rosters

(define (check-roster-for-subject? class rosters)
  (ormap
   ;;Roster->Boolean
   (lambda (et)(equal? class
                       (roster-classname et)))rosters)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-student-name-to-class:Enrollment SOR -> SOR
;; GIVEN: A enrollment and set of rosters to add enrollment to set of rosters
;; RETURNS:A Set of rosters with new student name added to class in set of rosters
;; EXAMPLE:Check test of enrollments-to-rosters for example
;; DESIGN-STRATERGY:Use HOF map on rosters
(define (add-student-name-to-class enrollment rosters)
  (map
   ;;Roster->Roster
   ;;DESIGN-STRATERGY:Use template of enrollment on enrollment
   (lambda (et)(append-student-to-class-list (enrollment-class enrollment)
                                             (enrollment-student enrollment)et))
   rosters))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; append-student-to-class-list: Class Student Roster -> Roster
;; GIVEN:A enrollment and a roster
;; RETURNS:A roster with student name added to that class
;; EXAMPLE:Check test for example
;; DESIGN STRATERGY:Use template of roster on roster 
(define (append-student-to-class-list  class student roster)
  (if(equal? (roster-classname roster) class)
     (make-roster (roster-classname roster)
                  (cons student
                        (roster-students roster)))
     roster))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
