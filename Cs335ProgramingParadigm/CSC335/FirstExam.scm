
; CSc 335
; Spring 2016

; March 8

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: Nelson Batista

; TYPE YOUR FULL EMAIL ADDRESS HERE: nbatist000@citymail.cuny.edu
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

(define (power b)
  (lambda (n) (cond ((= n 0) b)
                    (else ((- n 1) (*b b))))))

; 1b. (5 points) Show that you understand the point of writing an exponentiation function
; this way by using your solution to 1a to write a function which computes
; non-negative powers of 10.

; (No proof is required.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 2a.  (15 points) Write an iterative function
;        LargestIntegerSmallerThanNumWhichDoesNotContainDigits
; with integer arguments num, digit1, digit2, and digit3 which returns the largest integer
; smaller than num which contains none of the digits digit1, digit2, or digit3.  Thus
;      (LargestIntegerSmallerThanNumWhichDoesNotContainDigits 450 1 4 7) = 399.

(define (LargestIntegerSmallerThanNumWhichDoesNotContainDigits   digit1 digit2 digit3)
  (define (isvalid? n d1 d2 d3)
    (let ((dig (modulo n 10)))
      (cond ((or (= dig d1) (= dig d2) (= dig d3)) #f)
            ((< n 10) #t)
            (else (isvalid? (quotient n 10) d1 d2 d3)))))

  (cond ((isvalid? num digit1 digit2 digit3) num)
        (else (LargestIntegerSmallerThanNumWhichDoesNotContainDigits (- num 1) digit1 digit2 digit3))))

; 2b. (15 points) Prove that the function you write for 2a is correct.

; This function is indeed iterative, as the only values that need to be maintained to continue computation at any point are the arguments currently being passed.
; The function merely checks whether the current number contains any digits provided and, if not, checks the next lowest. The function must eventually terminate,
; since only 3 digits are provided, and we reduce the number checked by one on each iteration. There are nine digits in total, so there must be some number greater
; than n which does not contain any of the 3 digits provided.
; The function does not appear to have any preconditions. We previously showed that the loop will eventually terminate since there are nine digits, but only three
; are not allowed to appear in the number returned. There are a finite number of integers between any given number and zero, and the function checks each of them
; in turn for validity, starting at the number itself and then working its way down. Thus, the function must return a valid number which is as close as possible 
; to the number given.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 3a. (10 points) Write a recursive program reverse-number to reverse the digits of a number.
; For example,(reverse-number 0) = 0, (reverse-number 1234) = 4321.  You may assume that
; the input is a non-negative integer.

(define (reverse n)
  (define (tens n)
    (cond ((< n 10) 1)
          (else (* 10 (tens (quotient n 10))))))
  (cond ((< n 10) n)
        (else (+ (* (modulo n 10) (tens n)) (reverse (quotient n 10))))))

; 3b. (15 points) Prove by induction that a call (reverse-number n) of your function
; reverse-number actually does return the number with digits the same as those
; of n, but in reverse order.  You should give only one induction: if you use
; more than one recursion to solve the problem, prove only the most important.

; We first assume that, for an arbitrary input k, reverse (quotient n 10) will correctly return the value k', which is 
; k with its digits reversed. We observe that this holds true for k < 10, in the basis step, as the function simply returns k itself.
; Any number less than 10 will have only one digit and thus be itself reversed. We must also assume that, for some arbitrary input q,
; the function `tens q` will return 10^n, where n is the number of digits in q. If q < 10, the basis step, tens will return 1, since there is only 1 digit.
; otherwise, for q*10 (which has n+1 digits), the function returns 10*tens(quotient (* q 10) 10), which is 10*tens(q), which will turn into 10*(10^n), or 10^(n+1),
; which is correct since q*10 has n+1 digits. Thus, we know tens works correctly and does eventually terminate, since (quotient n 10) will return n/10,
; which eventually goes to 0 since each division produces a number with one less digit, and n/10 returns 0 if n < 10.
;
; reverse n returns n if n < 10, which is correct, since any number less than 10 will have a single digit, and a single digit reversed is simply itself.
; for some arbitrary n, we assume n returns the sum of each digit multiplied by 10^(x-1), x being its position within n. For instance, 1234 will return 1*10^0+2*10^1
; + 3*10^2+4*10^3. The function definitely terminates, since each recursive step takes the quotient of n and 10, which will always produce a number with one less digit.
; as the recursion continues, the number of digits must eventually reach 1, at which point the base case will be run, the deferred operations evaluated, and the
; function returned.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 4a. (5 points) Write a simple and reasonably efficient program prime? to determine
; whether an integer n > 0 is prime.

(define (prime? n)
  (define (iter n candidate)
    (cond ((= candidate  modulo n candidate) 0) #f)
          (else (iter n (- candidate 1)))))

  (iter n (floor (sqrt n))))

; 4b. (10 points) Write a Scheme function invariant-under-reverse? which inputs
; a predicate p and an integer n > 0 and which outputs the nth integer m for which
; both m and its reverse satisfy the predicate p.  Thus, if n = 5 and p = prime?,
; (invariant-under-reverse? prime? 5) = 11.
; Note that the reverse of a single digit number is that number itself; note that
; this function need not be iterative, despite its name; note that this problem
; builds on Problem 3.

; check if curr num and reverse curr num satisfy p. if so, increment count. if count == n, return currnum
; if not, check next number.

(define (invariant-under-reverse? p n)
  (define (iter p n count curr)
    (cond ((= count n) (- curr 1)) ; return the previous number since it was the n-th one that satisfied, not the current one
          ((and (p curr) (p (reverse curr))) (iter p n (+ count 1) (+ curr 1)))
          (else (iter p n count (+ curr 1)))))

  (iter p n 1 0))

; 4c. (15 points) Give a proof of the function you write for 4b.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




