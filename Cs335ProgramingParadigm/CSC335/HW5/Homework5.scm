

; Fifth Homework Set
; CSc 335
; Fall 2016


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework Problems

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; 1.  Prove the correctness of the value function presented in lecture9.scm.  Your argument should be an
; induction on (the complexity of) the argument.  State clearly what assumptions you make.

; I first assume that the functions natnum?, plus-aexp?, plus, times-aexp?, times, power-aexp?, expon, first-operand, and second-operand functions all work as intended and return the correct values. That is, natnum? aexp returns #t if aexp is a natural number and #f otherwise, plus-aexp? aexp returns true if the aexp is an expression containing a + operator, plus value1 value2 returns the sum of the values adjacent to the + operator in the aexp, and so on.
; I observe that, in the event that the provided aexp is simply a number, the natnum? aexp line in the cond block will cause value to simply return the expression itself. This is correct behavior, as there are no operators to evaluate. This is thus a useful basis step, as it represents the simplest possible aexp. This allows us to induct on the complexity of the aexp, using the case of the aexp being a single natural number as a basis step.
; We now assume that, in an expression containing an operator, the recursive call (value (first-operand aexp)) returns the correct value of the first operand of the operator, and similarly the call (value (second-operand aexp)) returns the correct value of the second operand. Since these calls would return natural numbers (the function stops when aexp is a natural number), and plus takes two natural numbers and combines them into a single number, the return value is necessarily less complex than the input value, since it contains one less operator. By our assumption that plus, times, and expon all work as intended (and, if not, it is not the responsibility of value to account for any incorrectness), these functions return a single value consisting of the operator applied to the two operands adjacent to it, which must be natural numbers as per our inductive hypothesis. This is exactly the behavior that our function should exhibit, and the result is an aexp with one less operand and thus one less level of complexity. As a given aexp must have a finite number of operands, value will eventually return a single value, the value of the aexp.

; 2.  Make all changes needed to allow the a-iexp calculator to work with numbers given in base 1.
; Explain carefully how you will represent such numbers; design plus, times, and exponent procedures
; for them.  Prove correctness of your functions.

; such numbers can be trivially represented by a list of 1s (or any non-reserved character, for that matter).
; For instance, the base 1 representation of the number 5 would be (1 1 1 1 1). For 2 it would be (1 1), for 10 it would be (1 1 1 1 1 1 1 1 1 1), etc.
; as dealing with such lists can be quite tedious, I simply wrote two functions, get-unary and create-unary which take a list of 1s and a value, respectively, and return the corresponding number in decimal and unary form, respectively. This allows us to minimize the changes required in the remaining functions by extracting the number and applying the corresponding operations to it, then converting the result back to unary.

; this function relies entirely on the assumption that the built-in function length returns the correct length of the provided list. Given this assumption, there is little to prove. Our representation of an unary number n is simply a list of n 1s, which necessarily has a length equal to n.
(define (get-unary num)
  (length num))

; if the number is 0, this function correctly returns an empty list, which can be loosely described as a list consisting of 0 1s.
; otherwise, it returns it returns a 1 appended to a list of n-1 1s, which is the correct return value since it will create a list of n 1s.
(define (create-unary num)
  (cond ((= num 0) '())
        (else (cons 1 (create-unary (- num 1))))))

(define add1
  (lambda (m) (create-unary (+ (get-unary m) 1))))

(define sub1
  (lambda (m) (create-unary (- (get-unary m) 1))))

; 3.  Exercises 2.2, 2.3 and 2.4 in Abelson and Sussman.  Proofs are not necessary.

; 2.2
(define (make-segment p1 p2)
  (list p1 p2))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (car (cdr line)))

(define (make-point x y)
  (list x y))

(define (x-point pt)
  (car pt))

(define (y-point pt)
  (car (cdr pt)))

(define (midpoint-segment line)
  (make-point (/ (+ (x-point (start-segment line)) 
                    (x-point (end-segment line)))
                 2)

              (/ (+ (y-point (start-segment line))
                    (y-point (end-segment line)))
                 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define p1 (make-point 0 0))
(define p2 (make-point 10 10))
(print-point p1)
(print-point p2)
(define l1 (make-segment p1 p2))
(print-point (midpoint-segment l1))

;2.4

(define (cons x y)
  (lambda (m) (m x y))) ; applies function m to arguments x y

(define (car z)
  (z (lambda (p q) p))) ; applies function z to first argument of 

(car (cons x y))
((cons x y) (lambda (p q) p))
((lambda (m) x y) (lambda (p q) p))
((lambda (p q) p) x y)
x

(define (cdr z)
  (z (lambda (p q) q)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

