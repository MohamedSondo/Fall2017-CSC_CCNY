;; Fall Exam 1

; Problem 1 (25 points) Write a scheme function expo of one integer argument b which returns a function of
; one argument e so that, for e a non-negative integer, ((expo b) e) returns b raised to the eth power.
; The function returned by (expo b) should work by repeated multiplication, and should be be properly
; recursive; give a proof that this function is correct.  Don't forget the termination argument.

(define (expo b)
  (define (inner e)
    (if (= e 0)
        1
        (* b (inner (- e 1)))))
  inner)

;; ((expo 2) 4)
; 
; Problem 2 (25 points) Design and certify an iterative scheme procedure count-digits which inputs a single
; non-negative integer n and which returns the number of digits in n.  Your proof should be based on an
; invariant.  Again, do not forget the termination argument.

(define (count-digits n)
  (if (<  n 0)
      #f
      (count-digits-iter n 0)))

(define (count-digits-iter number length)
  (if (= 0 number)
      length
      (count-digits-iter (quotient number 10) (+ length 1))))

;(count-digits -1)
;(count-digits 125)

;; Recursive implementation

(define (count-digitz n)
  (cond ((< n 0) #f)
        ((= n 0) 0)
        (else (+ 1 (count-digitz (quotient n 10))))))

;;(count-digitz -1)
;;(count-digitz 125)

; Problem 3 (40 points)  Write and certify a scheme function scramble with arguments n and f, where n is a
; positive integer and f is a function from the set {0,1,2,...,9} of digits to the set of non-negative
; integers, and which returns the number formed from n by replacing each digit j by the digits (in order)
; of the value (f j).

; Thus if f is the function which squares each digit, (scramble 403612 f) returns 16093614

; Your function can be either recursive or iterative, as you see fit: be sure to say which, and to give
; a proof (induction or invariant based) which matches your choice. Again, don't forget the termination argument.

; (Hint: work from the right, and perhaps make use of your function count-digits. Your proof, should you use
; count-digits, will need to show that the count-digits precondition holds each time it is called; your proof should also
; indicate how the post-condition of count-digits contributes to the main argument. )

(define (square x) (* x x))

(define (scramble n f)
  (if (= (count-digits n) 1) (f n)
      (scramble-iter n f 0 0)))

(define (scramble-iter num term length result)
  ;(display num)(display " ")(display length)(display " ")(display result)(newline)
  (cond ((= num 0) result)
        ((= (modulo num 10) 0)
         (scramble-iter (quotient num 10)
                        term
                        (expt 10 (count-digits length))
                        (+ result (* (expt 10 (count-digits length)) (term (modulo num 10))))))
        (else (scramble-iter (quotient num 10)
                             term
                             (+ length (* (expt 10 (count-digits length)) (term (modulo num 10))))
                             (+ result (* (expt 10 (count-digits length)) (term (modulo num 10))))))))

;;(scramble 9080403060102 square)


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
; power (you can use the scheme primitive, expt).

; No proofs are required for problem 4.

(define curry-2
  (lambda (x)
    (lambda (y)
      (lambda (z)
        (z x y)))))

;;(((curry-2 1) 2) +)

(define (expo-b b e)
  (expt b e))

;;(((curry-2 5) 3) expo-b)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spring Exam 1

; Problem 1 (25 points) Design and certify an iterative scheme procedure mymul which accepts integer inputs
; m >= 0 and n, and which returns the product of m and n.  Your procedure should use repeated addition
; to accomplish the multiplication; it should not use primitive multiplication procedures.  Your proof
; should be based on an invariant. Don't forget the termination argument.

(define (mymul m n)
  (if (< m 0)
      #f
      (mymul-iter m n 0)))

(define (mymul-iter m n result)
  (cond ((= n 0) result)
        (else (mymul-iter m (- n 1) (+ result m)))))

;;(mymul 5 3)

(define (my-mul m n) ;recursive
  (cond ((< m 0) #f)
        ((= n 0) 0)
        (else (+ m (my-mul m (- n 1))))))

;;(my-mul 5 3)

; Problem 2 (25 points) Use the equation (n - 1)^2 = n^2 -2n + 1 to develop a non-iterative, recursive scheme
; procedure which inputs an integer n >= 0 and which returns n^2.  Give correctness and termination arguments.

(define (square n)
  (cond ((< n 0) #f)
        ((= n 0) 0)
        (else (+ (square (- n 1)) (* 2 n) -1))))

;;(square 4)

; Problem 3 (25 points) Write and prove correct a scheme procedure which inputs a function f and integers
; a and b with a <= b and which outputs an integer k, a <= k <= b, such that the value f(k) of f at k is
; smallest among all values in the set {f(a), f(a+1), ... , f(b)}.  Specify how your procedure handles
; the case when there are several points k between a and b at which the value of f(k) is smallest.  Remember
; to include in your pre-condition whatever constraints you need to impose on the function parameter f;
; remember to give a termination argument.

(define (find-smallest f a b)
  (if (> a b)
      #f
      (find-smallest-iter f a b (f a))))

(define (find-smallest-iter f a b result)
  (cond ((> a b) result)
        ((< (f a) result) (find-smallest-iter f (+ a 1) b (f a)))
        (else (find-smallest-iter f (+ a 1) b result))))

(define (square-root x) (expt x 0.5))

;;(find-smallest square-root 25 100)

  
; Problem 4 (25 points) First, for 5 points (this was homework), write a procedure compose that implements
; function composition - for f and g functions of one argument, (compose f g) is the function which
; computes (f (g (x))).  For example, ((compose square inc) 6) = 49.
; NO PROOFS ARE REQUIRED FOR PROBLEM 4

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
  (define (inner num)
    (f (g num)))
  inner)

;;((compose square inc) 6)

; Second, for 5 points (this was homework), write a procedure that takes as inputs
; a procedure that computes a function f of one argument and a positive integer n and which returns the
; procedure that computes the nth repeated application of f. Your procedure should be able to be used as follows:

; ((repeated square 2) 5)
; 625

(define (repeated f x)
  (if (= x 0)
      (lambda (x) x)
      (compose f (repeated f (- x 1)))))

;;((repeated square 2) 5)


; Third, for 5 points, write a function deriv which takes two arguments - a function g and a small
; number dx - and which returns a function (deriv g dx) which approximates the derivative of g using
; the difference quotient

;  g(x + dx) - g(x)
; -----------------
;        dx


; Thus, assuming cube is the function (lambda (x) (* x x x)), ((deriv cube .0001) 5) will be a number
; quite close to 75.

(define (cube x) (* x x x))

(define (deriv g dx)
  (define (inner x)
    (/ (- (g (+ x dx)) (g x)) dx))
  inner)

;;((deriv cube .0001) 5)

; And finally, for 10 points, show how to use repeated and deriv to compute a function nth-deriv approximating
; the nth derivative of its input function.  That is, the call

; ((nth-deriv cube 2 .0001) 5)

; will be a number quite close to 30

(define (nth-deriv g repeat dx)
  (define (inner x)
    ((deriv g x) ((repeated g repeat) dx)))
  inner)

;;((nth-deriv cube 2 .0001) 5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spring 2016

; 1a. (10 points) Write a Scheme function power of one integer argument b which
; returns a function of one integer argument n so that

; ((power b) n)

; is b to the nth power.  Thus ((power 10) 3) = 1000.  You may assume n >= 0; a
; design requirement is that the function have no unnecessary parameter passing.

; (No proof is required.)

(define (power b)
  (define (inner n)
    (expt b n))
  inner)

;;((power 10) 3)

; 1b. (5 points) Show that you understand the point of writing an exponentiation function
; this way by using your solution to 1a to write a function which computes
; non-negative powers of 10.

(define (compute-power-ten n)
  ((power 10) n))

;;(compute-power-ten 3)

; 2a.  (15 points) Write an iterative function
;        LargestIntegerSmallerThanNumWhichDoesNotContainDigits
; with integer arguments num, digit1, digit2, and digit3 which returns the largest integer
; smaller than num which contains none of the digits digit1, digit2, or digit3.  Thus
;      (LargestIntegerSmallerThanNumWhichDoesNotContainDigits 450 1 4 7) = 399.

(define (num-part-of? num1 digit1 digit2 digit3)
  (if (or (equal? num1 digit3) (equal? num1 digit2) (equal? num1 digit1))
      #t
      #f))


(define (LargestIntegerSmallerThanNumWhichDoesNotContainDigits num digit1 digit2 digit3)
  (define (check-num num-left)
    (cond ((= 0 num-left) num)
          ((num-part-of? (modulo num-left 10) digit1 digit2 digit3)
           (LargestIntegerSmallerThanNumWhichDoesNotContainDigits (- num 1) digit1 digit2 digit3))
          (else (check-num (quotient num-left 10)))))
  (check-num num))

;;(LargestIntegerSmallerThanNumWhichDoesNotContainDigits 450 1 4 7)
;;(LargestIntegerSmallerThanNumWhichDoesNotContainDigits 597 1 4 7)

; 3a. (10 points) Write a recursive program reverse-number to reverse the digits of a number.
; For example,(reverse-number 0) = 0, (reverse-number 1234) = 4321.  You may assume that
; the input is a non-negative integer.

(define (myexpt b e)
  (cond ((= e 0) 1)
        (else (* b (myexpt b (- e 1))))))

(define (get-length num)
  (cond ((= num 0) -1)
        (else (+ 1 (get-length (quotient num 10))))))

(define (reverse-number num)
  (cond ((= num 0) 0)
        (else (+ (* (modulo num 10) (expt 10 (get-length num)))
                 (reverse-number (quotient num 10))))))

;;(reverse-number 0)
;;(reverse-number 1234)
;;(reverse-number 12345734534538)

; 4a. (5 points) Write a simple and reasonably efficient program prime? to determine
; whether an integer n > 0 is prime.

(define (check-square-root n count)
  (if (= count n)
      #f
      (if (= (* count count) n)
          #t
          (check-square-root n (+ 1 count)))))

(define (prime? n)
  ;(display n)(newline)
  (cond ((< n 0) #f)
        ((or (= n 1) (= n 0)) #f)
        ((or (= n 7) (= n 5) (= n 3) (= n 2)) #t)
        ((even? n) #f)
        ((= (modulo n 3) 0) #f)
        ((= (modulo n 5) 0) #f)
        ((= (modulo n 7) 0) #f)
        (else (if (check-square-root n 0)
                  #f
                  #t))))

;(prime? 15)
;(prime? 16)
;(prime? 17)

; 4b. (10 points) Write a Scheme function invariant-under-reverse? which inputs
; a predicate p and an integer n > 0 and which outputs the nth integer m for which
; both m and its reverse satisfy the predicate p.  Thus, if n = 5 and p = prime?,
; (invariant-under-reverse? prime? 5) = 11.

; Note that the reverse of a single digit number is that number itself; note that
; this function need not be iterative, despite its name; note that this problem
; builds on Problem 3.

(define (verify p n)
  ;(display "Verify => ")(display n)(display " and ")(display (reverse-number n))(newline)
  (cond ((< n 0) #f)
        ((and (p n) (p (reverse-number n)))
         #t)
        (else #f)))

(define (get-the-prime-number p goal count current)
  ;;(display "prime number is => ")(display current)(newline)
  ;;(display (prime? current))(display " | ")(display count)(newline)
  (cond ((= count goal) current)
        ((and (p current) (= (+ 1 count) goal))
         (get-the-prime-number p goal (+ 1 count) current))
        ((p current)
         (get-the-prime-number p goal (+ 1 count) (+ 1 current)))
        (else
         (get-the-prime-number p goal count (+ 1 current)))))

(define (invariant-under-reverse? p n)
  ;(display "The prime number is ")(display (get-the-prime-number p n 0 0))(newline) ; if you want to see the value
  (verify p (get-the-prime-number p n 0 0)))

;;(invariant-under-reverse? prime? 10)
;;(invariant-under-reverse? prime? 20)
;;(invariant-under-reverse? prime? 30)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fall 2016

; 1.  (15 points) Recursive patterns lurk everywhere.  Use the equation (n + 1)^2 = n^2 + 2n + 1 to develop a
; recursive program for computing n^2.  Explain the connection between the equation and your program.

(define (n-square n)
  (cond ((<= n 0) 0)
        (else (+ (n-square (- n 1)) (* 2 n) -1))))

;;(n-square 5)

; 2a.  (15 points) Write a properly recursive Scheme function num-digits-satisfying which inputs
; an integer n and a boolean function test? of one argument and which returns the number of digits
; in n for which (test? n) is true.  Thus the call (num-digits-satisfying 152535 (lambda (n) (= n 5)))
; should return 3, and (num-digits-satisfying -152535 odd?) would return 5.

(define (num-digits-satisfying n f)
  (if (= n 0)
      0
      (if (f (modulo n 10))
          (+ 1 (num-digits-satisfying (quotient n 10) f))
          (num-digits-satisfying (quotient n 10) f))))

;;(num-digits-satisfying 152535 (lambda (n) (= n 5)))
;;(num-digits-satisfying -152535 odd?)

; 3a.  (15 points) Now rewrite num-digits-satisfying so that it generates an iterative process.

(define (iter-num-digits-satisfying n f)
  (num-digits-satisfying-iter n f 0))

(define (num-digits-satisfying-iter n f result)
  (if (= n 0)
      result
      (if (f (modulo n 10))
          (num-digits-satisfying-iter (quotient n 10) f (+ result 1))
          (num-digits-satisfying-iter (quotient n 10) f result))))

;;(iter-num-digits-satisfying 152535 (lambda (n) (= n 5)))
;;(iter-num-digits-satisfying -152535 odd?)

; 4. (15 points) Write a function make-repeated which inputs a function f of one argument and which returns a function
; g of two arguments b and n so that ((make-repeated f) b n) applies f n times to b.  Demonstrate your function by 
; showing how to use it to apply the function square 8 times to 2. 

; Translation - keep reapplying square of the result on the result eight times. (Honestly spent more time thinking
; what his question even wants, damn English.)

(define (square x) (* x x))

(define (make-repeated f)
  (define (inner b n)
    ;;(display b)(newline)
    (if (= n 0)
        b
        (inner (f b) (- n 1))))
  inner)
  
;;((make-repeated square) 2 8)




  