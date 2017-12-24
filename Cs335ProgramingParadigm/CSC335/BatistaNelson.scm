; CSc 335
; Fall 2016

; September 29

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: Nelson Batista

; TYPE YOUR FULL EMAIL ADDRESS HERE: nbatist000@citymail.cuny.edu
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (max 15 points)  12

;;;; Problem 2a - code (max 15 points)  15
;;;; Problem 2b - proof (max 20 points) 19

;;;; Problem 3 - code (max 15 points)  15
;;;; Problem 3 - proof (max 20 points) 10

;;;; Problem 4 - code (max 15 points)  15



;;;; Total  86
;;;; Letter Grade  A

;;;; good job

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


; 1.  (15 points) Recursive patterns lurk everywhere.  Use the equation (n + 1)^2 = n^2 + 2n + 1 to develop a
; recursive program for computing n^2.  Explain the connection between the equation and your program.  

(define (formula n)
  (define (abs x)
    (if (< x 0) (- 0 x)
        x))
  
  (cond ((= n 0) 0)
        (else (+ (formula (- (abs n) 1))
                 (* 2 (- (abs n) 1))
                 1))))

(formula -3)
(formula 0)
(formula 10)
(formula 7)
(formula -7)
(newline)

(de)
(formula -3)
(formula 0)
(formula 10)
(formula 7)
(formula -7)
(newline)
; the function takes advantage of the fact that, since (n+1)^2 = n^2 + 2n + 1, we must have n^2 = (n-1)^2 + 2(n-1) + 1 = (n-1)^2 + 2n - 1
; using zero as a base case, as well as the knowledge that (-n)^2 = n^2, we can create a recursive function which returns 0 if n = 0, and
; (|n|-1)^2 + 2|n| - 1 otherwise. The function must terminate, since subtracting 1 from the absolute value of any finite number will eventually
; yield zero. Our base case is indeed consistent with the formula given, as 0^2 = (0-1)^2 + 2(0) - 1 = 1-1 = 0.

;;;; good !

;;;; I would point out that you might want to compute (abs n) just once, at the top.  As it is now, (formula -3) returns -3, instead of 9



; 2a.  (15 points) Write a properly recursive Scheme function num-digits-satisfying which inputs an integer n and a boolean function
; test? of one argument and which returns the number of digits in n for which (test? n) is true.  Thus the call
; (num-digits-satisfying 152535 (lambda (n) (= n 5))) should return 3, and (num-digits-satisfying -152535 odd?) would 
; return 5.

(define (num-digits-satisfying test? n)
  (define (bool-to-num b)
    (cond (b 1)
          (else 0)))

  (define (abs x)
    (if (< x 0) (- 0 x)
        x))
  
  (cond ((< n 10) (bool-to-num (test? n)))
        (else (+ (bool-to-num (test? (modulo (abs n) 10)))
                 (num-digits-satisfying test? (quotient (abs n) 10))))))










(define (num-digits-satisfying n test?)
  (define (bool-to-num b)
    (cond (b 1)
          (else 0)))

  (define (abs n)
    (cond ((< n 0) (- 0 n))
          (else n)))
  
  ; single digit, return whether it satisfies test?
  (cond ((< (abs n) 10) (bool-to-num (test? (abs n))))
        ; otherwise do rightmost digit and add to result of (num-digits-satisfying (rest of number))
        (else (+ (bool-to-num (test? (modulo (abs n) 10)))
                 (num-digits-satisfying (quotient (abs n) 10) test?)))))

(num-digits-satisfying -152535 odd?)
(num-digits-satisfying 152535 (lambda (n) (= n 5)))
(newline)


;(define (num-digits-satisfying n test?)
;  (define (abs n)
;    (cond ((< n 0) (- 0 n))
;          (else n)))
;  
;  (define (check test? num)
;    (cond ((test? num) 1)
;          (else 0)))
;
;  (cond ((< (abs n) 10) (check test? n))
;        (else (+ (num-digits-satisfying (quotient (abs n) 10) test?) (num-digits-satisfying (modulo (abs n) 10) test?)))))


;;;;  good



; 2b.  (20 points) Give a complete proof showing that your function num-digits-satisfying is correct.
; We begin with a base case. In the event that n is less than 10 (and thus must contain only one digit), the function returns
; 1 if the digit satisfies test? or 0 otherwise. This is correct. We do not concern ourselves with whether test? returns within
; finite time, as this has no bearing on the correctness of our algorithm.

;; We then induct on the number of digits in n. We first assume that, for a number containing k digits, the recursive call
;; (num-digits-satisfying (quotient (abs k) 10) test?) returns the correct number of digits in (quotient k 10) which satisfy test?.
;; We need not make such an assumption for (num-digits-satisfying (modulo (abs k) 10) test?) since (modulo x 10) always returns
;; a single-digit number, which we have already seen num-digits-satisfying handles correctly. Thus, this correct value is added
;; to the result of the recursive call, which we assume is correct, giving the number of digits in k which satisfy test?
;; for a number containing k+1 digits, we note that (quotient x 10) always returns a number with one less digit than x.
;; Thus, the recursive call (num-digits-satisfying (quotient (abs (+ k 1)) 10) test?) simply returns the number of digits in
;; in (quotient (abs (+ k 1)) 10) which satisfy test?, but (quotient (abs (+ k 1)) 10) is a number with k digits, for which we
;; know num-digits-satisfying returns the correct value.


; The function is properly recursive, as the recursive call includes a deferred operator in the form of the addition. The stack must be used to keep track
; of each recursive call, before adding them to get the final result.

; We first use the induction hypothesis that, for some arbitrary input k, the recursive call (num-digits-satisfying (quotient (abs k) 10) test?) returns
; the correct number of digits in (quotient (abs k) 10) that satisfy the function test?

; We observe that, if |n| < 10 (absolute value in the event that the first call to the function uses a negative value for n, the function returns 1 if n
; satisfies test? and 0 otherwise. This is certainly correct. If n is only one digit, num-digits-satisfying should return at most 1; whether that digit
; satisfies the condition or not. The function thus works for the base case, in which n < 10.

; For n >= 10, we use our induction hypothesis to assume that (num-digits-satisfying (quotient (abs k) 10) test?) returns the correct value: the number of
; digits in (quotient (abs k) 10) that satisfy n. This value is added to (num-digits-satisfying (modulo (abs n) 10) test?), which we observed returns the
; correct value for all input n, since (modulo n 10) will always return a single-digit value. Thus the function adds the result of applying itself to 
; the least significant digit of n, which we know returns the correct value, and the result of applying itself to the remaining digits of n. Since each
; call removes one digit from n, and n has a finite number of digits, the function will eventually be forced to add the result of itself applied to each
; individual digit of n, which is the correct value since the function is to return the number of digits in n which satisfy test?.

;;;; you want to mention that (test? n) needs to terminate as well. 


; 3a.  (15 points) Now rewrite num-digits-satisfying so that it generates an iterative process.

(define (num-digits-satisfying test? n)
  (define (bool-to-num b)
    (cond (b 1)
          (else 0)))

  (define (abs x)
    (cond ((< x 0) (- 0 x))
          (else x)))
  
  (define (iter test? n result)
    (cond ((< n 10) (+ result (bool-to-num test? n)))
          (else (iter test?
                      (quotient n 10)
                      (+ result (bool-to-num (test? (modulo n 10))))))))

  (iter test? (abs n) 0))










(define (num-digits-satisfying n test?)
  (define (bool-to-num b)
    (cond (b 1)
          (else 0)))

  (define (abs n)
    (cond ((< n 0) (- 0 n))
          (else n)))
  
  (define (iter n test? result)
    (cond ((< n 10) (+ result (bool-to-num (test? n))))
          (else (iter (quotient n 10)
                      test?
                      (+ result (bool-to-num (test? (modulo n 10))))))))

  (iter (abs n) test? 0))

(num-digits-satisfying -152535 odd?)
(num-digits-satisfying 152535 (lambda (n) (= n 5)))
(newline)
;(define (num-digits-satisfying n test?)
;  (define (check test? num)
;    (cond ((test? num) 1)
;          (else 0)))
;  
;  (define (iter n test? count)
;    (cond ((= n 0) count)
;          (else (iter (quotient n 10) test? (+ count (check test? (modulo n 10)))))))
;
;  (iter n test? 0))

; 3b.  (20 points) Give a complete (invariant-based) proof showing that your rewritten num-digits-satisfying is correct. 

; We first note that the function will certainly terminate within finite time, since the iteration calls itself using the quotient of n and 10, which
; necessarily produces a number with one less digit than n. Since all numbers have a finite number of digits, the number of digits will eventually become 1,
; at which point (quotient n 10) will return 0, causing the iter function to return its current count.


;;;; again, what if (test? n) fails to terminate?


; On each iteration,the count variable of iter contains the number of digits in the processed portion of the original value n which satisfy test?. On each
; iteration, one more digit (the least significant) is evaluated by test?, and count is updated. Using a similar argument as that in the recursive
; implementation, we know all digits will eventually be processed, since (quotient n 10) returns a number with one less digit. The digit removed is processed,
; so count will eventually contain the total number of digits in n which satisfy the predicate, which is exactly what the function is supposed to return.


;;;; you give an invariant, but not the invariant-based argument which should build on it. 


; 4. (15 points) Write a function make-repeated which inputs a function f of one argument and which returns a function
; g of two arguments b and n so that ((make-repeated f) b n) applies f n times to b.  Demonstrate your function by 
; showing how to use it to apply the function square 8 times to 2.

(define make-repeated
  (lambda (f)
    (lambda (b n)
      (cond ((= n 0) b)
            (else ((make-repeated f) (f b) (- n 1)))))))

;(define (make-repeated f)
;  (define (use f x y)
;    (cond ((= y 0) x)
;          (else (use f (f x) (- y 1)))))
;
;  (lambda (b n) (use f b n)))


;;;; it works!


(define (square x) (* x x))
  
((make-repeated square) 2 8)

;((lambda (b n) (apply square b n)) 2 8)
;(use square 2 8)
;(use square 4 7) ; (square 2)
;(use square 16 6) ; (square (square 2))
;(use square 256 5) ; (square (square (square 2)))
;(use square 65536 4)
;(use square 4294967296 3)
;(use square 18446744073709551616 2)
;(use square 340282366920938463463374607431768211456 1) ; square applied 7 times to 2 = 340282366920938463463374607431768211456
;(use square 115792089237316195423570985008687907853269984665640564039457584007913129639936 0) ; square applied
; 8 times to 2 = 115792089237316195423570985008687907853269984665640564039457584007913129639936