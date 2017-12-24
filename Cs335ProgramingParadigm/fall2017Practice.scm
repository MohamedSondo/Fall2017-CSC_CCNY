(define (myabs x)
  (cond((< x 0)(- x))
       (else x)))

(myabs -5)


(define x 6)
(cond ((> x 6) 1)
      ((< x 6) 2 )
      (else 0))




(define (sqr x)
  (cond ((= x 0)0)
        (else(> x 0)(* x x))))
(sqr 5)

(define (abss x)
  (if (< x 0)(- x)(x)))
(abss -5)


(define (sum-of-square-two-largest x y z)
  (cond ((= x (min x y z))(+(sqr y) (sqr z)))
         ((= y (min x y z))(+(sqr x)(sqr z)))
          (else (+ (sqr x ) (sqr y)))))

(define (lrgIntSmNCd num digit1 digit2 digit3)
  (define (isvalid? n d1 d2 d3 )
    (let((dig (modulo n 10)))
      (cond (( or (= dig  d1) (= dig d2) (= dig d3)) #f)
            (( < n 10) #t)
            (else (isvalid? (quotient n 10) d1 d2 d3)))))
  (cond ((isvalid? num digit1 digit2 digit3)num)
        (else(lrgIntSmNCd(- num 1) digit1 digit2 digit3))))
(lrgIntSmNCd 450 1 4 7)


; this function is indeed iterative, as the only value that need to be maintenained to contrinue the computation
; are the argument being pass to the function. the function check whether the current number contain any of the given digit, if not
; the function is then call on the next lowest number to repeat the process. Since there are only 3 digit
; out of nine in total, so there must be some number greater. the loop will eventuaky termine as this can be considered the termination argument for this program given that no precondition has been  provide.
; they are finite number of interger and our function isvalid check for the validity of these number and return the close number to the given number.

(define (DigitSpread n)
  (define (findDigit n op)
    (cond((= (quotient n 10) 0)n)
         (else(op (modulo n 10)(findDigit(quotient n 10) op)))))
  (-(findDigit n max) (findDigit n min)))

(DigitSpread 12345678)
;; the function indee work as a recursibve solotion for find the spread of digit in a given number n.,
;; Given the precondition for n as the set of all natural number, we wish to computer the spread of digit between n
  ;; assming that scheme work, out program will produce our desire output given that our precond old.
; for the base case, any number that is less than 10, th digit spread is one.
;; we forulate our inductuction hypothesis that for abibrary k value,DigitSpread( k) return the correct value for digit spreak
;; wwe now show that a since aprogra, work on small input, by the principal of mathematical induction the pogram will work on large input.
;; that is that for k+1 Digit Spread return the correct value. That in indeed true, with our function findDigit we are recursively loop for the greater or min number between n mod 10 and the quotient .
;; since out base case alway schech our recursion will continue as long as the quotient and n is not 0;
;; (-(findDigit K+1 max) ( findDigit K+1 min))) will return the different between the ,min and the max.
;; it remaind to prove that our program will eventually halp once the precondition is no longer satify.
;; as we are recursively finding the min and max of digit through repeated division and  mod operation, we are reducing the numebr digit in n and n will eventualy reach the base and will halt.

(define (sum-digit-recur num)
  (cond(( <= num 0)0)
       (( < num 10)num)
       (else(+ (sum-digit-recur ( quotient num 10))(modulo num 10)))))
(sum-digit-recur 1234)
;; this function return the sum of digit given that our precond hold. that is that n is an interger and greater than 0. for k
;; value, we have the base of our recursion which return the 0. we in assume that for abitrary k+1 the right sum is returned.
;(sum-digit-recur ( quotient num 10))(modulo num 10))) the mod operation give us the the last value in the number. and the quotient with n eventually give us fewer remainde digit. for  mod of k give last dit and  and quotient let l be the lengh of the digit
;quotient return l-1 and this is done recursive until the number of digit end. this step also illustrate our termination argumnent thjat our program will evenetuall halt.