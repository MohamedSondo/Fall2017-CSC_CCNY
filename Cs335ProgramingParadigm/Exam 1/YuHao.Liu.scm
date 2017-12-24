

; CSc 335
; Spring 2017

; March 9

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: Yu Hao Liu

; TYPE YOUR FULL EMAIL ADDRESS HERE: liuyuhao717@gmail.com/yliu006@citymail.cuny.edu
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 8 points)  8
;;;; Problem 1 - proof (out of 7 points   7
;;;; Problem 1 - synergy: proof and code developed at the same time (out of 10 points)  10

;;;; Problem 2 - code (out of 10 points)  10
;;;; Problem 2 - proof (out of 10 points)   3
;;;; Problem 2 - synergy: proof and code developed at the same time (out of 10 points)  2

;;;; Problem 3 - code (out of 10 points)   6
;;;; Problem 3 - proof (out of 15 points)    0
;;;; Problem 3 - synergy: proof and code developed at the same time (out of 10 points)  0

;;;; Problem 4 - code (out of 10 points)  4

;;;; Total  50
;;;; Letter Grade  C

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework: no lists, no strings, no assignment.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  


; Problem 1 (25 points) Write and certify a properly recursive scheme function DigitSpread of one integer
; argument n which returns the difference between the largest and smallest digits occurring in n.  Don't forget the termination argument.

;;;; INSERT YOUR ANSWER HERE
(define (digitspread n)
  (- (process-max n (remainder n 10)) (process-sml n (remainder n 10))))
(define (process-sml n small)
    (cond ((< n 10) (if (< n small) n small))
          (else (if (< small (process-max (quotient n 10) (remainder n 10))) small (process-sml (quotient n 10) (remainder n 10))))))

(define (process-max n larger)
    (cond ((< n 10) (if (> n larger) n larger))
          (else (if (> larger (process-max (quotient n 10) (remainder n 10))) larger (process-max (quotient n 10) (remainder n 10))))))

(digitspread 1234)
(digitspread 1)


;;;; nice



;proof
;basic case when n is one digit number, it will return 0, according to code, process-max return n and process-sml return n as well since
; they both are not greater than / smaller than itself
; IH: Assume kth call is work, process-max return the max, and process-sml return the smallest
; IS: when k+1th call, (> process-max(kth call) significant digit(base case)) which return the correct max. Which also same as smallest digit.
; pre-condition is n is a integer post-condition is return the difference between largest and smaller
; Termination: the recursion stop when n is less than 10 which is one digit



;;;; clean and efficient. Good!


; Problem 2 (30 points) Write and certify an iterative scheme procedure count-digits which inputs a single
; non-negative integer n and which returns the number of digits in n.  Your proof should be based on an
; invariant.  Again, do not forget the termination argument.    


;;;; INSERT YOUR ANSWER HERE
(define (count-digits n)
  (define (process n count)
    (cond ((< n 10) (+ count 1))
          (else (process (quotient n 10) (+ count 1)))))
  (process n 0))
(count-digits 1)
(count-digits 100)
; proof
; The guess invariant is the number of digits in n = count



;;;; clearly not right on initial call, when count = 0 and n is the input integer.  This is the point of the 'synergy' component to the grade: you are supposed to
;;;; pay extra careful attention to the synchrony of your code and your proof.

;;;; it is a shame here that you were not more careful, because - though I can't give you many points for the proof - you clearly have a good idea about at least
;;;; the form of invariant-based proofs. 



; first-test: When n less than 10, according to the code, number_in_d = 0 + 1 = 1.
; second-test: first time call. (process (quotient n 10) (+ count 1)) return the total of number_in_digit. when n = two digit number.
; (process (quotient n 10) (+ count 1)) returns 2 by everytime when n quotient 10, count adds one. And n will become only one digits, and the
; first-test will occur and count adds one more. Which return 2.
; Third-test: Asume kth call is work => k+1th call work. On the kth call, (process (quotient n 10) (+ count 1)) return count. For the k+th call
; (process (quotient n 10) (+ count 1) will run one more time. Therefore return count + 1.
; pre-condition n is a integer and post-condition return the number_digit in n
; termination: n is less than 10


; Problem 3 (35 points)  Write and certify a scheme function scramble with arguments n and f, where n is a
; positive integer and f is a function from the set {0,1,2,...,9} of digits to the set of non-negative
; integers, and which returns the number formed from n by replacing each digit j by the digits (in order)
; of the value (f j).

; Thus if f is the function which squares each digit, (scramble 403612 f) returns 16093614

; Your function can be either recursive or iterative, as you see fit: be sure to say which, and to give
; a proof (induction or invariant based) which matches your choice. Again, don't forget the termination argument.

; (Hint: work from the right, and perhaps make use of your function count-digits. Your proof, should you use
; count-digits, will need to show that the count-digits precondition holds each time it is called; your proof should also
; indicate how the post-condition of count-digits contributes to the main argument. )

;;;; INSERT YOUR ANSWER HERE
(define (scramble n f)
  (define (scramble n f g count)
    (cond ((< n 10) (* (f n) (expt 10 (- (+ count (g n)) 1))))
          (else (+ 0 (scramble (quotient n 10) f g (+ (count-digits (f (remainder n 10))) count))))))
  (scramble n f count-digits 0))

(define (square x)
  (* x x))
(scramble 4 square)
; the idea is everytime when we quotient n by 10 we need to make sure the it times with power of 10 with count-digits ( f (remainder n 10)), the pow of 10
; is to make sure about the position of digits.


;;;; yes, but as you have it now, (scramble 1234 square) returns 10000.

;;;; you do have a lot of the right pieces, though -- I can look at this as a preliminary design.

;;;; no proof





; Problem 4 (10 points) The function

(define add-1
  (lambda (y) (+ y 1)))

; can be generalized

(define add-x
  (lambda (x)
    (lambda (y)
      (+ x y))))

; so that add-1 can be realized as (add-x 1).

; In this problem, you are asked to go a step further, and generalize this pattern to allow its use with any function
; of two arguments (not just +).  Name the new function curry-2, and show how to use it to realize add-x.  Show
; further how to use it to define a function expo-b, which inputs a non-negative integer e and outputs b raised to the eth
; power (you may use the scheme primitive, expt).

; NO PROOFS ARE REQUIRED FOR PROBLEM 4




;;;; PUT YOUR ANSWER HERE
(define curry-2
  (lambda(f)
    (lambda(x)
      (lambda(y)
        ((f x) y)))))


;;;; almost: f was to have been binary, not unary.

;;; missing requested applications.





























