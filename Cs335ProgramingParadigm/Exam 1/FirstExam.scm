
; CSc 335
; Spring 2016

; March 8

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE:

; TYPE YOUR FULL EMAIL ADDRESS HERE:
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1a - code (max 10 points)
;;;; Problem 1b - code (max 5 points)

;;;; Problem 2a - code (max 15 points)
;;;; Problem 2b - proof (max 15 points)

;;;; Problem 3 - code (max 10 points)
;;;; Problem 3 - proof (max 15 points)

;;;; Problem 4a - code (max 5 points)
;;;; Problem 4b - code (max 10 points)
;;;; Problem 4c - proof (max 15 points)



;;;; Total
;;;; Letter Grade

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using the R5RS
; implementation provided by drracket and only those language features discussed so far in the
; context of lectures and homework.

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Here are the examination problems.  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1a. (10 points) Write a Scheme function power of one integer argument b which
; returns a function of one integer argument n so that

; ((power b) n)

; is b to the nth power.  Thus ((power 10) 3) = 1000.  You may assume n >= 0; a
; design requirement is that the function have no unnecessary parameter passing.

; (No proof is required.)

(display "\n Question 1 \n")
(define (power b)
  (define (raise n)
    (if (= n 0) 1
        (* b (raise (- n 1)))
    )
  )
  raise
)

((power 10) 3)

; 1b. (5 points) Show that you understand the point of writing an exponentiation function
; this way by using your solution to 1a to write a function which computes
; non-negative powers of 10.

(define (compute-power-10 x)
  (if (< x 0) #f
      ((power 10) x)
  )
)

(compute-power-10 3)

; (No proof is required.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 2a.  (15 points) Write an iterative function
;        LargestIntegerSmallerThanNumWhichDoesNotContainDigits
; with integer arguments num, digit1, digit2, and digit3 which returns the largest integer
; smaller than num which contains none of the digits digit1, digit2, or digit3.  Thus
;      (LargestIntegerSmallerThanNumWhichDoesNotContainDigits 450 1 4 7) = 399.

; 2b. (15 points) Prove that the function you write for 2a is correct. 

(display "\n Question 2 \n")
(define (check-part num1 digit1 digit2 digit3)
  (if (or (equal? num1 digit3) (or (equal? num1 digit2) (equal? num1 digit1) ) )
      #t
      #f
   )
  )

(define (LargestIntegerSmallerThanNumWhichDoesNotContainDigits num digit1 digit2 digit3)
  (define (check-inner num-left)
    (if (= num-left 0) num
      (if (check-part (modulo num-left 10) digit1 digit2 digit3)
          ;#t
          (LargestIntegerSmallerThanNumWhichDoesNotContainDigits (- num 1) digit1 digit2 digit3)
          ;#f
          (check-inner (quotient num-left 10))
      )
    )
  )
  (check-inner num)
)

(LargestIntegerSmallerThanNumWhichDoesNotContainDigits 450 1 4 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 3a. (10 points) Write a recursive program reverse-number to reverse the digits of a number.
; For example,(reverse-number 0) = 0, (reverse-number 1234) = 4321.  You may assume that
; the input is a non-negative integer.

(display "\n Question 3 \n")
(define (get-length n length)
  (if (= (quotient n 10) 0) length
      (get-length (quotient n 10) (+ length 1))
  )
)

(define (reverse-number value)
  (if (= value 0) 0
      (+ (reverse-number (quotient value 10)) (* (expt 10 (get-length value 0)) (modulo value 10)))
  )
)

(reverse-number 123456009)

; above is recursive

; now trying iterative

(define (reverse-iter n)
  (if (= (get-length n 0) 1) 1
      (reverse-it n 0 0 (+ (get-length n 0) 1))
  )
)

(define (reverse-it n result length final-length)
  (display n)(display " ")(display result)(display " ")(display length)(display " ")(display final-length)(display "\n")
  (if (= length final-length) result
      (reverse-it (quotient n 10) (+ result (* (expt 10 (get-length n 0)) (modulo n 10))) (+ length 1) final-length)
  )
)

(reverse-iter 123456)
; 3b. (15 points) Prove by induction that a call (reverse-number n) of your function
; reverse-number actually does return the number with digits the same as those
; of n, but in reverse order.  You should give only one induction: if you use
; more than one recursion to solve the problem, prove only the most important.

; properly recursive

; basis: induct on the num of digits in n
;         10^(num of digits) * (right-most digit) + reverse(num/10)

; hypothesis: given an integer n, the program will output the reversed interger

; induction:  if reverse-num (n) is reversed, then the digit at n+1 is the only remaining digit to reverse. So largest-index will be 0

; termination: eventually reverse-number n is equal to 0, terminating program.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 4a. (5 points) Write a simple and reasonably efficient program prime? to determine
; whether an integer n > 0 is prime.

(display "\n Question 4 \n")
(define (square-root n k)
  (if (= n k) #t
      (if (= (modulo n k) 0) #f
          (square-root n (+ k 1))
       )
  )
)

(define (prime? n)
  (cond ((or (= n 0) (= n 1)) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else (square-root n 2))
  )
)

(prime? 5)

; 4b. (10 points) Write a Scheme function invariant-under-reverse? which inputs
; a predicate p and an integer n > 0 and which outputs the nth integer m for which
; both m and its reverse satisfy the predicate p.  Thus, if n = 5 and p = prime?,
; (invariant-under-reverse? prime? 5) = 11.

(define (find-prime p n)
  (define (create-prime p value position final-position)
    (if (and (= position final-position) (equal? (p value) #t))
        (p (reverse-number value))
        (if (p value)
            (create-prime p (+ value 2) (+ position 1) final-position)
            (create-prime p (+ value 2) position final-position)
        )
    )
  )
  (cond ((or (= n 0) (= n 1)) (p (reverse-number n)))
        (else (create-prime p 3 2 n))
  )
)

(define (invariant-under-reverse? p n)
  (if (p n)
      (find-prime p n)
      #f
  )
)

;(prime? 461)
(invariant-under-reverse? prime? 19)

; Note that the reverse of a single digit number is that number itself; note that
; this function need not be iterative, despite its name; note that this problem
; builds on Problem 3.


; 4c. (15 points) Give a proof of the function you write for 4b.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




