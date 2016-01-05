;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "04" "class-lists.rkt")
(provide felleisen-roster)
(provide shivers-roster)
(define-struct slip (color name1 name2))
;;A Slip is a (make-slip Color String String)
;;Where Color is color of slip
;;name1 is first name
;;name2 is last name respectively
;;Template
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
;;CONSTANTS
(define slip1 (make-slip "yellow" "A" "B"))
(define slip2 (make-slip "yellow" "B" "C"))
(define slip3 (make-slip "yellow" "B" "A"))
(define slip4 (make-slip "blue" "B" "C"))
(define slip5 (make-slip "yellow" "C" "B")) 
(define slip6 (make-slip "yellow" "A" "B"))
(define slip7 (make-slip "blue"  "B" "B"))
(define slip8 (make-slip "blue" "B" "B"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;felleisen-roster : LOS -> LOS
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;Felleisen's class, without duplication.
;;EXAMPLE:Check test for example
;;DESIGN-STRATERGY:Use template for LOS on slips 

(define (felleisen-roster slips)
   (if(empty? slips)
      empty
     (remove-duplicate
      (sort-by-color slips YELLOW))))

;;TEST
(begin-for-test
  (check-equal? (felleisen-roster (list slip1 slip2 slip3 slip4 slip4
                                       slip5 slip6 slip7 slip8))(list slip5
                                                                      slip6)
                "Should return list of students on yellow slip")
  (check-equal? (felleisen-roster empty) empty
                "Should return emptylist"))                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;shivers-roster: LOS -> LOS
;;GIVEN: a list of slips
;;RETURNS: a list of slips containing all the students in Professor
;;Shivers' class, without duplication.
;;EXAMPLE:Check Tests for example
;;DESIGN-STRATERGY:Use template for LOS on slips
 (define (shivers-roster slips)
   (if(empty? slips) empty
     (remove-duplicate
       (sort-by-color slips BLUE))))
;;TEST
(begin-for-test
  (check-equal? (shivers-roster (list slip1 slip2 slip3 slip4 slip4
                                       slip5 slip6 slip7 slip8))(list slip4
                                                                      slip8)
                "Should return list of students on yellow slip")
  (check-equal? (shivers-roster empty) empty
                "Should return emptylist"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;remove-duplicate:LOS->LOS
;;GIVEN:A list of slip with or without duplicate records
;;RETURNS:A list of slip without duplicate records.
;;EXAMPLE:Check Test for example
;;DESIGN-STRATERGY:Use template for LOS on slips
(define (remove-duplicate slips)
   (cond
     [(empty? slips) empty]
     [else (if (duplicate? (slip-name1(first slips))
                               (slip-name2(first slips))
                               (rest slips))
               (remove-duplicate (rest slips))
               (cons (first slips)
                     (remove-duplicate (rest slips))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;duplicate?:String String LOS->Boolean
;;GIVEN:the first name ,last name and list of slips to check if there
;;are duplicate record in list of slip
;;RETURNS:a boolean value.True iff a duplicate record is found in list
;;else returns false.
;;EXAMPLE:Check test of felleisen-roster function or shivers-roster
;;function for example
;;DESIGN-STRATERGY:Use template of LOS on slips
(define (duplicate? firstname lastname slips)
  (if(empty? slips)
      false
  (or (check-for-duplicate-name? firstname lastname
           (first slips))
           (duplicate? firstname lastname
                           (rest slips)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-for-duplcate-name?:String String Slip->Boolean
;;GIVEN:Firstname,Lastname and slip
;;RETURNS:returns true if firstname is equal to lastname and lastname is equal
;;to firstname
;;EXAMPLE:Check test of felleisen-roster function or shivers-roster function for
;;example.
;;DESIGN-STRATERGY:Use template for Slip on slip
(define (check-for-duplicate-name? firstname lastname slip)
  (or (and (string=? firstname (slip-name2 slip)) 
       (string=? lastname  (slip-name1 slip)))
      (and (string=? firstname (slip-name1 slip)) 
       (string=? lastname  (slip-name2 slip)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;sort-by-color:LOS color->LOS
;;GIVEN:A list of slips and a color according to which the list is to be sorted
;;RETURN:A list of slips sorted by color
;;EXAMPLE:Check the test of felleisen-roster function or shivers-roster function
;;for example
;;DESIGN-STRATERGY:Use template for LOS on slips
(define (sort-by-color slips color)
  (cond
    [(empty? slips) empty]
    [else (if (string=? color (slip-color(first slips)))
          (cons (first slips)(sort-by-color (rest slips) color))
          (sort-by-color (rest slips)color))]))

