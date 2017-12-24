



;; These are some of the questions given in the past exams as a final, don't use this as what will be on the final exam.
;; Furthermore, this was given as the final BEFORE the professor decided there will be a third exam
;; that covers the TLS stuff. So these questions were final questions expected only for TLS.
;; Therefore, this is just a good reference for the third exam and a portion of the final exam.

;; TL;DR The name of this file is misleading, only use for third exam/part of final exam.

;1.  Show that TLS-Scheme correctly implements lexical scope.

;	tls-scheme uses local variable "table" to evaluate function. When outter function finish evaluation with its local table, 
;	the inner function will create another local table to evaluate itself. Functions have different tables which Make
;	TLS-Scheme lexical scope.



;2.  Show that TLS-Scheme correctly implements first class functions.


; Write, and prove correct, a syntax checker for TLS-Scheme.  Use the inductive definition of TLS-Scheme given in lecture15.scm.  You
; will need to give a complete specification for your program.

; I note that this problem has occurred as a final exam problem in CSc 335 in a past semester.

; Problem 3. Modify tls-scheme so that it correctly evaluates scheme expressions 
; containing let.  As you would by now expect, you need to give a complete and careful
; description and certification of your modifications; do not neglect the possibility 
; that occurrences of let may be nested.  Again, a key piece is to say what you mean
; by correctness.


; Problem 4 (10 points) Use a well-chosen tls-scheme expression to illustrate how the tls-scheme interpreter 
; provides variable scoping by describing - at an appropriate level of detail - the 
; evaluation of your expression.  Half of your grade on this problem depends on your choice
; of example - choose one which covers all aspects of lexical scoping, and say what you consider
; these to be.


; Problem 3a (15 points)   Make all modifications to the tls-scheme interpreter needed to 
; extend the language with an if construct.  Here if is a special form:
; (if test then-exp else-exp) first evaluates test; if test is true, then-exp is evaluated,
; but not else-exp; if test if false, else-exp is evaluated, but not then-exp.  For example,
; (if #t 1 (/ 1 0)) and (if #f (/ 1 0) 1) both return 1, with the division by zero never attempted
; in either case.