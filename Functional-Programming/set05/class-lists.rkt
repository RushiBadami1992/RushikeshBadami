;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Goal:To use higher order functions to implement class list problem.We have to filter the
;;slips according to color and remove duplicate from each slip. 
(require rackunit)
(require "extras.rkt")
(check-location "05" "class-lists.rkt")
(provide felleisen-roster)
(provide shivers-roster)
(provide
 make-slip
 slip-color
 slip-name1
 slip-name2)
(define-struct slip (color name1 name2))
;;A Slip is a (make-slip Color String String)
;;Where Color is color of slip
;;name1 is first name
;;name2 is last name respectively
;;Template
;;slip-fn: Slip->??
;;(define (slip-fn slip)
;; (...
;; (slip-color slip)
;; (slip-name1 slip)
;; (slip-name2 slip))     
  
;;A Color is one of
;;--yellow
;;--blue
;; where yellow is color of Professor Felleisen's slip
;; where blue is color of Professor Shivers'slip

;; TEMPLATE
;; color-fn : Color -> ??

;;(define (color-fn color)
;; (cond
;;   [(string=? color "yellow")    ...]
;;   [(string=? color "blue") ...]))

;; A ListOfSlips (LOS) is either
;; -- empty
;; -- (cons Slip LOS)

;; los-fn : LOS -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (...
;;             (slip-fn (first los))
;;             (los-fn (rest los)))]))
  
(define BLUE "blue")
;;represents blue color
(define YELLOW "yellow")
;;represents yellow color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants for testing
(define slip1 (make-slip "blue"  "C" "B"))
(define slip2 (make-slip "blue"  "B" "C"))
(define slip3 (make-slip "blue"  "C" "D"))
(define slip4 (make-slip "blue"  "D" "C"))
(define slip5 (make-slip "blue"  "K" "B"))
(define slip6 (make-slip "blue" "B" "K"))
(define slip7 (make-slip "yellow" "D" "K"))
(define slip8 (make-slip "yellow"  "C" "B"))
(define slip9 (make-slip "yellow"  "B" "C"))
(define slip10 (make-slip "yellow"  "C" "D"))
(define slip11 (make-slip "yellow"  "D" "C"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;felleisen-roster : LOS -> LOS
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;Felleisen's class, without duplication.
;;EXAMPLE:Check test for example
;;DESIGN-STRATERGY:Combine Simpler function. 

(define (felleisen-roster slips)
  (remove-duplicate
       (sort-by-color slips YELLOW)))
;;TEST
(begin-for-test
 (check equal? (felleisen-roster (list slip8 slip9 slip10 slip11 ))
               (list slip9 slip11)
                "Should return list of students on yellow slip")
  (check equal? (felleisen-roster empty) empty
                "Should return emptylist"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;shivers-roster: LOS -> LOS
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;Shivers' class, without duplication.
;;EXAMPLE:Check Tests for example
;;DESIGN-STRATERGY:Combine Simpler Function
 (define (shivers-roster slips)
  (remove-duplicate
       (sort-by-color slips BLUE))) 

;;TEST
(begin-for-test
  (check equal? (shivers-roster (list slip1 slip2 slip3 slip4 slip5 slip6))
                (list slip2 slip4 slip6)
                "Should return list of students on yellow slip")
  (check equal? (shivers-roster empty) empty
                "Should return emptylist"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;remove-duplicate:LOS->LOS
;;GIVEN:A list of slip with or without duplicate records
;;RETURNS:A list of slip without duplicate records.
;;EXAMPLE:Check Test of felleisen-roster or shivers-roster for example 
;;DESIGN-STRATERGY:Use  hof folder on slips

(define (remove-duplicate slips)
  (foldr
   add-unique-slip-to-list
   empty
   slips))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-unique-slip-list: Slip LOS -> LOS
;; GIVEN:-A list of slip and a slip to check whether it is present in list of slips 
;; RETURNS:-A list of slip without duplicates
;; EXAMPLE:-Check Test of felleisen-roster or shivers-roster for example
;; DESIGN-STRATERGY:Combine Simpler Functions
(define (add-unique-slip-to-list slip slips)
 (if (duplicate? slip slips)
      slips
     (cons slip slips)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;duplicate?:Slip LOS->Boolean
;;GIVEN:the first name ,last name and list of slips to check if there
;;are duplicate record in list of slip
;;RETURNS:a boolean value.True iff a duplicate record is found in list
;;else returns false.
;;EXAMPLE:Check test of felleisen-roster function or shivers-roster
;;function for example
;;DESIGN-STRATERGY:Use hof ormap on slips
(define (duplicate? slip slips)
  ;;Slip->Boolean
 (ormap
    (lambda (et)(check-for-duplicate-name? slip et))
    slips))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-for-duplicate-name?:Slip Slip ->Boolean
;;GIVEN:Two slips to compare if one is duplicate of another
;;RETURNS:returns true if firstname is equal to lastname and lastname is equal
;;to firstname respectively
;;EXAMPLE:Check test of felleisen-roster function or shivers-roster function for
;;example.
;;DESIGN-STRATERGY:Use template for Slip on slip
(define (check-for-duplicate-name? slip1 slip2)
  (or(and (string=? (slip-name1 slip1) (slip-name2 slip2)) 
          (string=? (slip-name2 slip1)  (slip-name1 slip2)))
     (and (string=? (slip-name1 slip1) (slip-name1 slip2)) 
          (string=? (slip-name2 slip1)  (slip-name2 slip2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;sort-by-color:LOS color->LOS
;;GIVEN:A list of slips and a color according to which the list is to be sorted
;;RETURN:A list of slips sorted by color
;;EXAMPLE:Check the test of felleisen-roster function or shivers-roster function
;;for example
;;DESIGN-STRATERGY:Use hof filter on slips

(define (sort-by-color slips color)
  (filter
   ;;Slip->Slip
   (lambda (slip)(string=? (slip-color slip) color))
   slips))

