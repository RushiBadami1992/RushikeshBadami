;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;            PROGRAM DESCRIPTION
;The rules for rendering the expression as a list of lines are as follows:
;
;The expression should be rendered on a single line if it fits within the specified width.
;Otherwise, render the subexpressions in a stacked fashion, that is, like
;	(+ expr1
;	   expr2
;	   ...
;	   exprN)
;and similarly for difference expressions.
;All subexpressions must fit within the space allotted minus the space for surrounding
;parentheses, if any. Apply the rendering algorithm recursively if needed.
;Note: there should be no spaces preceding a right parenthesis.
;The algorithm may determine that the given expression cannot fit within the allotted
;In this case, the algorithm should raise an appropriate error, using the function error.
;In addition to expr-to-strings, you must provide make-sum-exp, sum-exp-exprs,
;make-diff-exp, and diff-exp-exprs.
;
;

(require "extras.rkt")
(require rackunit)
(provide expr-to-strings
         make-sum-exp
         sum-exp-exprs
         make-diff-exp
         diff-exp-exprs)
(check-location "08" "pretty.rkt")
(define-struct sum-exp (exprs))

;; Sum-exp is  a (make-sum-exp LOExpr)
;; Interpretation:
;; a sum-exp represents a sum
;; LOExpr is the list of expressions. 
;; Template for sum-exp:
;; sum-exp-fn :Sum-exp->??
;; (define (sum-exp-fn se)
;; (.....(sum-exp-exprs se)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A LOExpr  is one of
;; -- empty
;; -- (cons Expr LOExpr)
;;  loexpr-fn: LOExpr->??

;; (define (loexpr-fn expr)
;;  (cond
;;  [(empty? expr)...]
;;  [(else
;;          (...(first expr)
;;           (loexpr-fn(rest expr)))]))

;; An Expr is one of
;; -- Integer
;; -- (make-sum-exp NELOExpr)
;; -- (make-diff-exp NELOExpr)
;; Interpretation: a sum-exp represents a sum and a diff-exp
;; represents a difference calculation.
;; TEMPLATE
;; expr-fn : Expr -> ??

;;(define (expr-fn expr)
;; (cond
;;   [(number? expr)...]
;;   [(sum-exp? expr) ...(sum-exp-fn expr)]
;;   [else  ...(diff-exp-fn expr)]))  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A NELOExpr(non-empty LOExpr) is one of
;; --(cons Expr empty)
;; --(cons Expr NELOExpr)

;; Template1
;; nelo-fn:NELOExpr-??
;; (define (nelo-fn neloe)
;; (cond
;;  [(empty? (rest neloe))...(first neloe)]
;;  [else (...(first neloe)
;;           (nelo-fn (rest neloe)))]))

;; Template2
;; nelo-fn:NELOExpr??
;; (define (nelo-fn neloe)
;; (...(first neloe)
;;           (nelo-fn (rest neloe))))
;;  
;; A ListOfString(LOS) is non-empty
;; --(cons String LOS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct diff-exp (exprs))
;; Diff-exp is  a (make-diff-exp LOExpr)
;; Interpretation:
;; represents a difference calculation.
;; LOExpr is the list of expressions.

;; Template for diff-exp:
;; diff-exp-fn :Diff-exp->??
;; (define (diff-exp-fn de)
;; (...(diff-exp-exprs de)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ZERO 0)
(define PREV 0)
(define NEXT 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expr-to-strings : Expr NonNegInt -> ListOfString
;; GIVEN: An expression and a width
;; RETURNS: A representation of the expression as a sequence of lines, with
;; each line represented as a string of length not greater than the width
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Combine Simpler Functions
(define (expr-to-strings  expr width)
 (pretty-print(trim-spaces-first-bracket(convert-expr-to-string expr "")PREV NEXT) width))

;; Test
(begin-for-test
(check-equal?  (expr-to-strings example 20) output-list2
  "Should return the following list")
(check-equal?  (expr-to-strings example 50) output-list1
  "Should return the following list")
(check-equal?(expr-to-strings  example 100)output-list4
 "Should return the following list"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert-expr-to-string: Expr String -> String
;; GIVEN: A Expr and a string to convert expr to string.
;; WHERE:A str is the string used to convert Expr to string 
;; RETURNS:The expression converted to string
;; EXAMPLE:Check test for example
;; DESIGN-STRATERGY:Combine Simpler Functions
(define (convert-expr-to-string expr str)
  (if
   (sum-exp? expr)
   (more-general-expr-to-string (sum-exp-exprs expr)(string-append  str " (+"))
   (more-general-expr-to-string (diff-exp-exprs expr)(string-append  str " (-"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; more-general-expr-to-string:Expr String->String
;; GIVEN: A expression and a String
;; RETURNS: A String with given expression and its list of expression converted to string
;; WHERE : str is a string which is used to convert a expression or list of expression to
;; string
;; EXAMPLE:Check test for example
;; DESIGN-STRATERGY:Use template of NELOExpr on expr

(define (more-general-expr-to-string expr str)
  (cond
    [(empty? expr)(string-append str ")")]
    [else (string-append  (condition-on-expr  (first expr) str)
                          (more-general-expr-to-string (rest expr)  ""))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; condition-on-expr: Expr String->String
;; GIVEN:A Expr and a String .
;; RETURNS:A number converted to string if it is a number.
;; EXAMPLE:Check Test for example
;; DESIGN-STRATEGY:recur on Diff-Expr or Sum-Expr if top level is not a number
;; Halting-Measure:When Expr is number return string.
(define (condition-on-expr expr str)
  (cond
    [(number? expr)(string-append str " "(number->string expr))]
    [(sum-exp? expr)
     (convert-expr-to-string expr str)]
    [(diff-exp?  expr)
     (convert-expr-to-string expr str)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate-closing-bracket:String NonNegInt->NonNegInt
;; GIVEN:A String and a counter to count closing bracket
;; RETURNS:A counter which counts number of brackets
;; WHERE:Count is used to count number of closing brackets in an expression
;; EXAMPLE:"(+ 22 33 44 54)"=1
;; DESIGN-STRATERGY:Recur on string till we get closing bracket
;; Halting-Measure:Return count when string-length is zero.

 (define (calculate-closing-bracket  str  count)
  (cond
    [(= (string-length str) 0) count] 
    [(equal? (substring str 0 1) ")")
    (calculate-closing-bracket  (substring str 1 (string-length str)) (+ count 1))]
    [else(calculate-closing-bracket  (substring str 1 (string-length str)) count)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pretty-print : String NonNegInt->LOS
;; GIVEN: A String and width which is greater than zero
;; RETURNS:A ListOfStrings with spaces for pretty printing
;; EXAMPLE: "(+ 222 33 44)" =(list "(+22" "33" "44"))
;; DESIGN-STRATEGY:Combine Simpler Functions
 
(define (pretty-print exprstr width)
  (if (< (string-length exprstr) width)
      (cons exprstr empty)
      (travel-list (divide-string-bracket-number exprstr
                                                 (calculate-start-of-word exprstr 0))
                   width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate-start-of-word:String->NonNegInt
;; GIVEN: A string with spaces and a counter
;; RETURNS:Total number of spaces before the string
;; EXAMPLE: "   Hello" = 3
;; DESIGN-STRATERGY:Combine Simpler Functions

(define (calculate-start-of-word exprstr counter)
  (if (equal? (substring exprstr PREV NEXT) " ")
      (calculate-start-of-word (substring exprstr NEXT (string-length exprstr))
                               (+ NEXT counter))
      counter))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; travel-list: LOS NonNegInt-> LOS
;; GIVEN: A ListOfString and width.
;; RETURNS:A ListOfString after breaking the string according to the width
;; EXAMPLE:Check test for example
;; DESIGN-STRATERGY:Use template of LOS on los

(define (travel-list los width)
(cond
  [(empty? los) empty]
  [else (append (pretty-print (first los) width) (travel-list (rest los) width))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trim-spaces-first-bracket:String NonNegInt NonNegInt->String
;; Given:A string and two counters first and second
;; RETURNS:A string with space trimmed before starting bracket
;; EXAMPLE:"  (+ 22" ="(+ 22"
;; DESIGN-STRATERGY:Combine Simpler Functions

(define (trim-spaces-first-bracket exprstr first second)
  (if
   (equal? (substring exprstr first second) " ")
   (substring exprstr second (string-length exprstr))
   exprstr))

(begin-for-test
  (check-equal? (trim-spaces-first-bracket " (+ 22" 0 1)
                "(+ 22"
                "Should trim spac before first bracket")
  (check-equal? (trim-spaces-first-bracket "(+ 22" 0 1)
                "(+ 22"
                "Should trim spac before first bracket"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; divide-string-bracket-number: String NonNegInt->LOS
;; GIVEN:A String and counter to count number of spaces
;; RETURNS:A LOS with counter placed at given location
;; EXAMPLE:Check Test for example
;; DESIGN-STRATERGY:Combine Simpler Functions.

(define (divide-string-bracket-number exprstr counter)
  (if (= (calculate-closing-bracket exprstr PREV) NEXT)
      (operations-break-number exprstr counter)
    (cons(operations-break-bracket exprstr counter)
           (operations-break-bracket-rest-list exprstr counter))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations-break-number: String Counter -> LOS
;; GIVEN:A expression and counter to convert the string into ListOfString after adding
;; spaces
;; RETURNS:A ListOfString after adding spaces
;; WHERE:Counter is used to keep the count of spaces at each depth.
;; DESIGN-STRATEGY:Combine Simpler Functions
;; EXAMPLE:Check test of exprs-to-string

(define (operations-break-number exprstr counter)
   (add-spaces-to-list (add-brackets-number
                         (break-number-on-space exprstr  PREV NEXT PREV counter)
                         empty)(+ counter 3)))
(begin-for-test
  (check-equal? (divide-string-bracket-number "(+ 222 44 33)" 0)
                (list  "   (+ 222 44 33)")
                "Should return following output"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations-break-bracket:String counter-> LOS
;; GIVEN:A string and a counter to add spaces in rest of list.
;; RETURNS:A los of strings with  spaces added to it
;; WHERE:Counter is used to keep the count of spaces at each depth.
;; EXAMPLE:Check test of exprs-to-string
;; DESIGN-STRATEGY:Combine Simpler Functions
(define (operations-break-bracket exprstr counter)
  (append-three-spaces (string-append (substring exprstr counter (+ counter 3))
                                             (first (get-list-of-string
                                                        (substring exprstr (+ 3 counter)
                                                        (- (string-length exprstr) NEXT))
                                                         PREV NEXT PREV))) PREV counter))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations-break-bracket-rest-list:String counter->LOS
;; GIVEN:A string and a counter which has number of spaces.
;; RETURNS:A LOS with spaces added to each elements.
;; EXAMPLE:Check test of exprs-to-string
;; DESIGN-STRATEGY:Combine simpler functions
(define (operations-break-bracket-rest-list exprstr counter)
  (add-spaces-to-list(rest(add-last-bracket(get-list-of-string
                                                        (substring exprstr (+ 3 counter)
                                                        (- (string-length exprstr) NEXT))
                                                         PREV NEXT PREV)))(+ counter 3)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-brackets-number:LOS LOS ->LOS
;; GIVEN:Two list of strings
;; RETURNS:A List of string with bracket appended to number
;; EXAMPLE: (list "(+" "22")=(list "(+ 22")
;; DESIGN-STRATERGY:Use template of LOS on los 

(define (add-brackets-number los ls)
  (cond
    [(number? (string->number (first los)))(cons (first los) ls)]
    [else (append ls (list (string-append(first los)" " (second los)))
                 (rest(rest los)))]))

(begin-for-test
  (check-equal? (add-brackets-number (list "1") empty)(list "1")
   "Should add element to list"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-spaces-to-list:LOS NonNegInt-> LOS
;; GIVEN: A LOS and a NOnNegInt integer
;; RETURNS:A list of string with three spaces appended to it.
;; EXAMPLE:Check test for example
;; DESIGN-STRATEGY:Use template of LOS on los
(define (add-spaces-to-list los counter)
  (cond
    [(empty? los) los]
    [else(cons (append-three-spaces (first los) PREV counter)
               (add-spaces-to-list (rest los) counter))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; append-three-spaces: String  NonNegInt NonNegInt->String
;; GIVEN:A String and two integers to count the number of spaces added
;; WHERE: initial is invariant used to keep the count of spaces added to string
;; RETURNS:A String with spaces added to it
;; DESIGN-STRATEGY:Combine Simpler Function.
(define (append-three-spaces str initial counter)
  (if (>= initial counter )
      str
      (append-three-spaces(string-append " " str) (+ initial NEXT) counter )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-last-bracket: LOS->LOS
;; GIVEN:A ListOfString to add last bracket to list of string
;; RETURNS:A ListOfString with last bracket added to it.
;; EXAMPLE:Check test for example
;; DESIGN-STRATERGY:Use template of LOS on los.

 (define (add-last-bracket los)
 (cond
     [(empty? los) empty]
     [else (cons (if (empty? (rest los))
                     (string-append (first los) ")")
                     (first los))
                 (add-last-bracket (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; break-number-on-space: String NonNegInt NonNegInt NonNegInt NonNegInt->ListOfString
;; GIVEN:A String the previous and next counters
;; WHERE: prev,next and initial are counter use to extract character from string 
;; RETURNS:A ListOfStrings after breaking it according to number
;; EXAMPLE:Check test for example
;; DESIGN-STRATERGY:Cases on closing bracket ")"
(define (break-number-on-space str prev next initial counter)
  (if (equal? (substring str prev next) ")")
     (cons str empty) 
     (check-for-other-bracket-operations str prev next initial counter)))
;;;;
(begin-for-test
  (check-equal? (break-number-on-space "(+ 222 44 33)"  0 1 0 1)
                (list "(+" "222" "44" "33)")
                "Should return the following list"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-for-other-bracket-operations :String NonNegInt NonNegInt NonNegInt NonNegInt->LOS
;; GIVEN:A String previous,next,initial and counter to count spaces
;; RETURNS:A LOS after breaking the string according to number
;; WHERE:prev,next and initial are counter use to extract character from string.Initial is
;; counter use to keep track of spaces before the start of string.
;; EXAMPLE:Check Test of break-number-on-space.
;; DESIGN-STRATEGY:Cases on string length
(define  (check-for-other-bracket-operations str prev next initial counter)
 (if
    (and (> (string-length str) next)
         (equal? (substring str prev next)" "))
    (check-for-preceeding-space  str prev next initial counter)
    (break-number-on-space str (+ prev NEXT)(+ next NEXT) initial counter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-for-preceeding-space:String NonNegInt NonNegInt NonNegInt NonNegInt->LOS
;; GIVEN: A string and three counters prev,next,initial,counter
;; RETURNS:A ListOfStrings after breaking them according to space
;; WHERE:prev,next and initial are counter use to extract character from string.Initial is
;; counter use to keep track of spaces before the start of string.
;; EXAMPLE:Check Test of break-number-on-space.
;; DESIGN-STRATEGY:Cases on value of initial counter

(define (check-for-preceeding-space  str prev next initial counter)
  (if (<= initial counter 0)
        (break-number-on-space str (+ prev NEXT) (+ next NEXT)(+ initial NEXT) counter)
        (cons (trim-at-last(substring str  PREV next))
              (break-number-on-space
                              (substring str next (string-length str)) 0 1 0 
                            counter ))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; trim-at-last:String->String
;; GIVEN:A string
;; RETURNS:A string whoose spaces are trimmed after bracket
;; EXAMPLE:"(+ 22 3333 44) "= "(+ 22 3333 44)"
;; DESIGN-STRATERGY:Combine Simpler Functions.

(define (trim-at-last str)
 (if
  (equal? (string-ith str (- (string-length str) NEXT)) " ")
  (substring str 0 (- (string-length str) NEXT))
  str))


(begin-for-test
  (check-equal? (trim-at-last "(+ 22 " )
                "(+ 22"
                "Should trim spac before first bracket")
  (check-equal? (trim-at-last "(+ 22" )
                "(+ 22"
                "Should trim spac before first bracket"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-list-of-string:String NonNegInt NonNegInt NonNegInt->LOS
;; GIVEN:A string a two counter to extract characters from a substring and track to keep
;; track of opening and closing bracket
;; WHERE : Previous and next are use to get single characters from string and track to keep
;; track of opening and closing brackets
;; EXAMPLE:Check test for example
;; DESIGN-STRATERGY:Recursing on character of string
;; Halting Measure:If string-length is zero return list.
(define (get-list-of-string exprstr prev next track)
  (cond
    [(= (string-length exprstr) PREV)empty]
  [else(cons (trim-first(get-part-of-string exprstr  prev next track))
                (get-list-of-string (substring exprstr (string-length
                                            (get-part-of-string exprstr  prev next track)))
                                    PREV NEXT PREV))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trim-first:String->String
;; GIVEN: A string
;; RETURNS:A string with spaces trimmed from strarting bracket
;; EXAMPLE: "  (+ 22" = "(+ 22)"
;; DESIGN-STRATEGY:Combine Simpler functions

(define (trim-first str)
  (if
   (equal? (substring str PREV NEXT) " ")
   (substring str NEXT (string-length str))
   str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-part-of-string: String NonNegInt NonNegInt NonNegInt->String
;; GIVEN:A String and previous and next counter to get character of string and track to
;; keep track of bracket
;; RETURNS:A bracket in the given expression
;; WHERE:Previous and next are use to get single characters from string and track to keep
;; track of opening and closing brackets
;; EXAMPLE:"(+ 22 4444 33)(77 88)"="(+ 22 4444 33)
;; DESIGN-STRATEGY:Cases on characters of string.
(define (get-part-of-string exprstr  prev next track)
  (cond
    [(equal? (substring exprstr prev next) "(")
     (get-part-of-string exprstr (+ prev NEXT)(+ next NEXT)(+ track NEXT))]
    [(equal? (substring exprstr prev next) ")")
     (return-substring exprstr prev next (- track NEXT))]
    [else (get-part-of-string exprstr (+ prev NEXT) (+ next NEXT) track)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; return-substring:String NonNegInt NonNegInt NonNegInt -> String
;; GIVEN:A String and previous and next counter to get character of string and track to
;; keep track of bracket.
;; RETURNS:A substring of given string
;; EXAMPLE:"(+ 22 3333 44) (77 88)"="(+ 22 3333 44)
;; DESIGN-STRATERGY:Combine Simpler functions.
(define (return-substring exprstr prev next track)
  (if(= track PREV)
     (substring exprstr PREV next)
     (get-part-of-string exprstr (+ prev NEXT)(+ next NEXT) track)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define example (make-sum-exp
 (list
  (make-diff-exp (list 22 3333 44))
  (make-diff-exp
   (list
    (make-sum-exp (list 66 67 68))
    (make-diff-exp (list 42 43))))
  (make-diff-exp (list 77 88)))))
(define output-list1 (list "(+ (- 22 3333 44)"
                           "   (- (+ 66 67 68) (- 42 43))" "   (- 77 88))"))
(define output-list2 (list "(+ (- 22 3333 44)" "   (- (+ 66 67 68)" "      (- 42 43))"
                           "   (- 77 88))"))
                          
(define output-list3 (list "(+ (- 22 3333 44)" "    (- (+ 66 67 68)" "       (- 42 43))"
                           "    (- 77 88))"))
(define output-list4 (list "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))"))
;;(trace expr-to-strings)