;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;*****************************************************************************************
;                            Program Description
;*****************************************************************************************
;The point of an outline is to impose a tree structure on a document, so it is natural to
;represent an outline as a tree. 
;Another representation of an outline could be as a list with one element per section or
;subsection.;Each element of the list would consist of two members: the section number,
;represented as a list of natural numbers, and a string.This would look more like the
;text representation. We call this the flat representation.

;*****************************************************************************************

(require rackunit)
(require "extras.rkt")
(provide legal-flat-rep?)
(provide tree-rep-to-flat-rep)
(check-location "07" "outlines.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                              DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct line (lon title))
;; Line is a(make-line LOPI String)
;; INTERPRETATION
;; lon is a ListOfPositiveInteger(LOPI) in flat representation which represent sections
;; number in the  representation.
;; title is the description of each section.

;; Template for Line:
;; line-fn :Line->??
;; (define (line-fn l)
;; (.....(line-lon l)
;;       (line-title l)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ListOfPositiveInteger(LOPI) is either
;; --empty
;; --(cons PosInt LOPI)

;; Template for ListOfPositiveInteger
;; lopi-fn:ListOfPositiveInteger->??
;; (define (lopi-fn numbers)
;; (cond
;; [(empty? numbers)...]
;; [else 
;;          (... (first numbers)
;;          (lopi-fn (rest numbers)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ListOfLines(LOL) is either
;; --empty
;; --(cons Line LOL)

;; Template for ListOfLines
;; lol-fn:ListOfLines->??
;; (define (lol-fn lines)
;; (cond
;; [(empty? lines)...]
;; [else ...(first lines)
;;          (lol-fn (rest lines)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct section(title sections))
;; section is a (make-section String LOS)
;; INTERPRETATION:
;; title is the header text of the section
;; sections is the list of subsections of the section. 

;; Template for Section:
;; section-fn: Section->??
;; (define (section-fn s)
;; (.....(section-title s)
;;       (los-fn (section-sections s))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ListOfSections (LOS) is either
;; -- empty
;; --(cons Section LOS)

;; Template for ListOfSections
;; los-fn:ListOfSections->??
;; (define (los-fn sections)
;; (cond
;; [(empty? sections)...]
;; [else (.....
;;          (section-fn (first sections)
;;          (los-fn (rest sections)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define DIFFERENCE-CONSTANT 1)
(define INITIAL-VALUE 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; legal-flat-rep? : ListOfLines -> Boolean
;; GIVEN: a list of lines.
;; RETURNS: true iff it is a legal flat representation of an outline.
;; EXAMPLE: Check test for example.
;; Strategy: Combine Simpler Functions

(define (legal-flat-rep? lol)
  (if
   (empty? lol)
   true
   (check-for-valid-outline? lol)))


;TEST
(begin-for-test
  (check-equal?
   (legal-flat-rep? (list (make-line (list 2) "The First Section")))
   #false)
  (check-equal? (legal-flat-rep? (list (make-line (list 1) "The First Section")))
                #true)
  (check-equal?
   (legal-flat-rep? (list))#true
   "Check when list is empty")
  (check-equal? (legal-flat-rep? flat-representation) #t
                "Should Return true as it is legal representation"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-for-valid-outline? ListOfLines -> Boolean
;;GIVEN: A list of lines .
;;RETURN: true iff it is legal flat representation.
;;EXAMPLE: (check-for-valid-outline? (list (make-line (list 2) "The First Section")))#f
;;STRATEGY: Use template for Line on (first lol).

(define (check-for-valid-outline? lol)
  (if
   (=(first(line-lon (first lol))) INITIAL-VALUE)
   (checking-entries?  empty lol)
   false))

;****************************************************************************************

;; checking-entries? :  ListOfPositiveInteger ListOfLines -> Boolean
;; GIVEN: a list of lines and list of numbers.
;; RETURNS: true iff there is only single entry or multiple entries by checking
;;          on the conditions given
;; WHERE: ListofLines is a non empty ListOfLines.
;; Example: testcases is covered in legal-flat-rep? function
;; Strategy: Use template for ListOfLines on (first lol)

(define (checking-entries?  lon lol)
  (cond
    [(empty? (rest lol))
     (check-outline-entries? lol lon)]
    [else (and (check-outline? (line-lon (first lol))(line-lon (second lol)))
               (checking-entries? (line-lon (first lol))(rest lol)))]))

;***************************************************************************************

;; check-outline-entries? : ListOfLines ListOfPositiveInteger-> Boolean
;; GIVEN:   a list of lines and list of numbers.
;; RETURNS: true iff there is only single entry or multiple entries by checking
;;          on the conditions given          
;; Example:  testcases is covered in legal-flat-rep? function
;; Strategy: Use template for line on (first lol)

(define (check-outline-entries? lol lon)
  (if
    (empty? lon)
    (check-for-single-line? lol) 
    (check-outline? lon (line-lon (first lol)))))

;*****************************************************************************************

;; check-for-single-line? : ListOfLines -> Boolean
;; GIVEN: a list of lines
;; RETURNS: true iff there is only single entry in the list of make-line,
;;          which will be 1
;; Example: (list (make-line (list 1) "The first section"))
;; Strategy: Use template for line on (first lol).

(define (check-for-single-line? lol)
  (and (= (length (line-lon (first lol))) DIFFERENCE-CONSTANT)
       (= (first (line-lon (first lol))) INITIAL-VALUE)))


(begin-for-test
  (check-equal?
   (check-for-single-line? (list (make-line (list 1)"the first section")))#true)) 

;*****************************************************************************************

;; check-outline? ListOfPositiveInteger ListOfPositiveInteger -> Boolean
;; GIVEN: two list of numbers .
;; WHERE:  both list  will be non-empty.      
;; RETURN: true iff the outline is followed based on the output of the
;;         functions called for each case.
;; Example (check-outline? (list 1 1 ) (list 1 2) #true)
;;         (check-outline? (list 1 1 2 ) (list 2) #true)
;;         (check-outline? (list 1 1 ) (list 1 1 1) #true)
;; Strategy : Divide on cases using lst1 and lst2.

(define (check-outline? lst1 lst2)
  (cond
    [(empty? lst2)false]
    [(=(length lst1) (length lst2))
     (check-for-incremented-length?  lst1 lst2)]
    [(= (- (length lst2)(length lst1)) 1)
     (check-for-equal-values-and-1? lst1 lst2)]
    [(< (length  lst2) (length lst1))
     (check-for-decremented-length? lst1 lst2)]))

;;Test
(begin-for-test
  (check-equal? (check-outline? (list 1 2 3) empty)#f
                "Should return false as list is empty"))

;**************************************************************************************

;check-for-incremented-length? ListOfPositiveInteger ListOfPositiveInteger -> Boolean
;GIVEN: two list of numbers .
;WHERE: length of the current list equal the previous list
;;      and both lists are non empty lists
;RETURN: true iff the the new section contains equal number of list elements
;        with last entry in the current list is 1 more than the last element
;;       in the previous list.
;Example (check-for-last-value-incremented? (list 1 1 ) (list 1 2) #true)
;Strategy : Combine simpler function.

(define (check-for-incremented-length? lst1 lst2)
  (if
   (equal? (rest(reverse lst1)) (rest(reverse lst2)))
   (=(- (first(reverse lst2 )) (first(reverse lst1))) DIFFERENCE-CONSTANT)
   false))


;TEST
(begin-for-test
  (check-equal?
   (check-for-incremented-length? (list 1 1) (list 1 1 1)) #false))

;*****************************************************************************************

;;check-for-equal-values-and-1? ListOfPositiveInteger ListOfPositiveInteger -> Boolean
;;GIVEN: two list of numbers .
;;WHERE: length of the current list is one more than the previous list
;;and both lists are non empty lists
;;RETURN: true iff the the new section contains one extra element in list which will be"1"
;;and every other  list entry matches with the previous list .
;;Example (check-for-equal-values-and-1? (list 1 1 ) (list 1 1 1) #true)
;;Strategy : Combine simpler function.

(define (check-for-equal-values-and-1? lst1 lst2)
  (if
   (equal? (reverse lst1) (rest(reverse lst2)))
   (= (first (reverse lst2)) INITIAL-VALUE)
   false))


;TEST

(begin-for-test
  (check-equal?
   (check-for-equal-values-and-1? (list 1 1) (list 1 3)) #false))

;****************************************************************************************
;;check-for-decremented-length? ListOfPositiveInteger ListOfPositiveInteger -> Boolean
;;GIVEN: two list of numbers .
;;WHERE: length of the current list is less than the previous list and both lists
;;are non empty lists
;;RETURN: true iff the the list-reference of the last element in current list wrt previous
;;       list is incremented by 1.
;;Example: (check-for-decremented-length? (list 1 1 ) (list 2) #true)
;;Strategy : Combine simpler function.

(define (check-for-decremented-length? lst1 lst2)
  (if
   (equal? (reverse(extract-list-one-elements lst1 (- (length lst2) DIFFERENCE-CONSTANT)
                                              DIFFERENCE-CONSTANT))
           (rest(reverse lst2)))
   (= (- (list-ref lst2 (- (length lst2) DIFFERENCE-CONSTANT))
         (list-ref lst1 (- (length lst2) DIFFERENCE-CONSTANT))) DIFFERENCE-CONSTANT)
   false))


;TEST
(begin-for-test
  (check-equal? 
   (check-for-decremented-length? (list  1 1 1)  (list 2 1)) #false))

;*****************************************************************************************

;; extract-list-one-elements: ListOfPositiveInteger PosInt PosInt -> ListOfPositiveInteger
;; GIVEN: a list of numbers.Length of second list and a counter to return the sublist of
;;        first list
;; WHERE:n is the counter which counts and returns a sublist of first
;;       list upto length of second list 
;; RETURN: A sublist of first list
;; Example:(extract-list-one-elements (list 1 2 3 4) 2 1)=(list 1 2) 
;; DESIGN-STRATEGY: : Use template of ListOfPositiveInteger on lst1

(define (extract-list-one-elements lst1 len n)
  (cond
    [(empty? lst1) empty]
    [else (if
           (>= len n)
           (cons (first lst1)(extract-list-one-elements (rest lst1) len
                                                        (+ n DIFFERENCE-CONSTANT)))
           (extract-list-one-elements (rest lst1) len (+ n DIFFERENCE-CONSTANT)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-rep-to-flat-rep : Outline -> FlatRep
;; GIVEN: the representation of an outline as a list of Sections
;; RETURNS: the flat representation of the outline.
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY: Combine simpler Functions

(define (tree-rep-to-flat-rep los)
  (trees-rep-to-flat-rep los (list INITIAL-VALUE)))

;;TEST
(begin-for-test
  (check-equal?
   (tree-rep-to-flat-rep sections-list) flat-representation
   "Should convert it to flat representation"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; trees-rep-to-flat-rep:ListOfSections ListOfPositiveInteger -> ListOfLines
;; GIVEN: the representation of an outline and a list of numbers starting from 1
;; RETURN: A list of lines representing list of sections in flat format
;; WHERE: lon is a list of numbers which represent section number in flat representation
;; DESIGN-STRATEGY: Use template of ListOfSections on los

(define (trees-rep-to-flat-rep los lon)
  (cond
    [(empty? los) empty]
    [else (append(convert-trees-to-flat (first los) lon)
                 (trees-rep-to-flat-rep (rest los)
                                        (reverse(increment-list-value (reverse lon)
                                               (length lon) INITIAL-VALUE))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert-trees-to-flat:Section ListOfPositiveInteger -> LOL
;; GIVEN: A section to be converted to flat representation.
;; RETURNS:A list of lines converting the section to flat representation
;; WHERE:lon is a list of numbers which represent section number in flat representation
;; EXAMPLE:Check test for example.
;; DESIGN-STRATEGY: Use template of section on sect

(define (convert-trees-to-flat sect lon)
  (cons(convert-tree-to-flat sect lon)
       (trees-rep-to-flat-rep (section-sections sect)
                              (append lon (list INITIAL-VALUE)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; convert-tree-to-flat: Section ListOfPositiveInteger ->Line
;; GIVEN:A section and a list of numbers which represents section in flat representation.
;; RETURN:A line which is a representation of section in line representation.
;; WHERE:lon is a list of numbers which represent section number in flat representation
;; EXAMPLE:Check test for tree-rep-to-flat-rep function.
;; DESIGN-STRATEGY:Use template for section on s.

(define (convert-tree-to-flat s lon)
  (make-line lon  (section-title s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; increment-list-value: ListOfNumbers NonNegInt NonNegInt -> ListOfNumber
;; GIVEN:A list of number,length of list and a invariant n to keep count of list
;; WHERE:n is  keeping the count of  list elements.
;; RETURNS:A list of numbers with its last number incremented
;; EXAMPLE:(List 1 2 3 4)=(1 2 3 5)
;; DESIGN-STRATEGY:Use template for ListOfNumbers on lon

(define (increment-list-value lon len n)
  (cond
    [(empty? lon) empty]
    [else(if
          (= n DIFFERENCE-CONSTANT)
          (cons (+ (first lon) DIFFERENCE-CONSTANT)(increment-list-value (rest lon) len
                                                               (+ n DIFFERENCE-CONSTANT)))
          (cons (first lon)(increment-list-value (rest lon) len
                                                 (+ n DIFFERENCE-CONSTANT))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sections-list (list 
                       (make-section "The first section"
                                     (list
                                   (make-section "A subsection with no subsections" empty)
                                      (make-section "Another subsection"
                               (list
                                 (make-section "This is a subsection of 1.2" empty)
                                (make-section "This is another subsection of 1.2" empty)))
                                (make-section "The last subsection of 1" empty)))
                                (make-section "Another section"
                                (list
                                      (make-section "More stuff" empty)
                                      (make-section "Still more stuff" empty)))))


(define flat-representation  (list
                              (make-line (list 1) "The first section")
                              (make-line (list 1 1) "A subsection with no subsections")
                              (make-line (list 1 2) "Another subsection")
                              (make-line (list 1 2 1) "This is a subsection of 1.2")
                              (make-line (list 1 2 2) "This is another subsection of 1.2")
                              (make-line (list 1 3) "The last subsection of 1")
                              (make-line (list 2) "Another section")
                              (make-line (list 2 1) "More stuff")
                              (make-line (list 2 2) "Still more stuff")))
;*****************************************************************************************
;                                  END OF PROGRAM
;*****************************************************************************************