(define (sigma a b)
  (cond((> a b) 0)
  (else(+ a (sigma( + a 1)b)))))


(define size 2)
(+ 5 size)

(define pi 3.14)
(define radius 10)
(* pi (* radius radius))


; function are value
(lambda(x) (* x x))
((lambda(x) (* x x))2 )


(define square(lambda(x) (* x x)))

(square 10)

(define x 3)
(define (add-x y) (+ x y))
(add-x 6)
; here x is global while y is local.

; function name can appear in other function also

(define (cube x)
  (* x (square x)))
(cube 2)
; here we see that square is global.
(define (sum-of-square x y)
  (+(square x)(square y)))
(sum-of-square 2 3)
; we can use sum-of-square as the building block for other function.


(define (f a)
  (sum-of-square (+ a 1)(* 2 a)))
(f 5)
; here we see that we use the function sum-of-square as a builing for a function that accep as input a and
; a value will be evalue in the parameter section of sum-of-square then sum-of-square is then compute.

; Conditional expression and predicate
(define (myabs x)
  (cond((> x 0) x)
	((= x 0) 0)
	(else (- x))))

(myabs 0)

(define (myabs x)
  (if (< x 0)
      (- x)
      x))
(myabs -8)

;

(define x 6)
( and (> x 5)(< x 10))
;the above function will eval to true

(not (> x 4))

(define(sum-square-of-two-largest x y z)
  (cond((= x (min x y z))(+(square y)(square z)))
       ((= y (min x y z))(+(square x)(square z)))
       (else(+ square x)(square y))))


(sum-square-of-two-largest 1 2 3)
; we will use let in for the function below.
(define (sum-square-of-two-largest-let x y z)
  (let((m (min x y z)))
    (cond((= m x)(+ (square y)(square z)))
         ((= m y)(+ (square x)(square z)))
         (else(+ (square x)(square y))))))
(sum-square-of-two-largest-let 1 3 4)
; let defines a local a local variable with scope just the body



(define (avg x y)
  (quotient
   (+ x y) 2))


(avg 6 9)

(define (warm temp)
  (if (> temp 75) "warm" "cold"))
(warm 50)


(define (cool temp)
  (cond
    ( (> temp 75) " hot")
    ( (< temp 65) "cold")
    ( else "ok")))
(cool 55)


(define (factT n )
  (factT-helper n 1))
(define (factT-helper z answer-so-far)
  (cond
    ((= z 1 ) answer-so-far)
    (else (factT-helper (- z 1) (*  z answer-so-far)))))
(factT  5)
; this function indeed return the correct value of the fac upon the call of factorioal at each recursive all
; every time when the function is called. For the base case, the correct value of fac is return upon the first call of the function]
;eventually the function value is retuned wihtout the function aving to waiting for each factorial call to finish beofore it multiple each function.
; once ANSWER SO FAR IS RETURN THE VALUE NO OTHER CALCULAITON TO BE COMPLETED.
 ; WE INDUCT ON THE NUMBER N  EACH TIME FACTT IS CALL THE PRECONDITION IS CHECKED, AND THE VALUE OF FACT IS EVALUATED IN  (factT-helper (- z 1) (*  z answer-so-far)
; WE CAN SEE THE VALUE OF Z IS DECREASE IN THE BODY OF HELPER FUNCTION.


