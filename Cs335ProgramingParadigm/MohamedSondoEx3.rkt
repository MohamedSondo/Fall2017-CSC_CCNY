;;; First practice start here
;;
(define pi 3.141592653)
(define radius 10)
;;circumference

(* pi(* radius radius))
(define (sqr x)(* x x))
(sqr 5)


(define sqrr (lambda(x) (* x x)))
(sqrr 9)


(define (sum-of-sqrr x y)
  (+ (sqrr x)(sqrr y)))
(sum-of-sqrr 3  4)

(- 10 3)

(define (formula n)
  (define (abs x )
    (if(< x 0)(- x) x))
  (cond((= n 0)0)
        (else(+(formula (- abs n ) 1))
               (* 2 (- (abs n)1)))))

;;;;;
(define atom?(lambda(x)
               (and(not(pair? x))(not(null? x)))))
(atom? 4)
;;;;;;;;;;;;;;;;;;;number does not contain d1 d2 d3


(define (LargestIntegerSmallerThanNumWhichDoesNotContainDigits num digit1 digit2 digit3)
  (display num)
  (newline)
  (define (isvalid? n d1 d2 d3)
    (let ((dig (modulo n 10)))
      (cond ((or (= dig d1) (= dig d2) (= dig d3)) #f)
            ((< n 10) #t)
            (else (isvalid? (quotient n 10) d1 d2 d3)))))

  (cond ((isvalid? num digit1 digit2 digit3) num)
        (else (LargestIntegerSmallerThanNumWhichDoesNotContainDigits (- num 1) digit1 digit2 digit3))))

(LargestIntegerSmallerThanNumWhichDoesNotContainDigits 450 1 4 7)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end d1 d2 d3.


;; a lat an s expression that is a list of atom.
(define lat?
  (lambda (l)
    (cond
      ((null? l)#t)
      ((atom? (car l))(lat?(cdr l)))
    (else #f))))
(lat? '(jack sprat could eat no chicken))#true
(lat? '(jack sprat '(could) eat no chicken))#false


;; menber return true if the argument a exist in the list of atom lat
(define menber?
  (lambda(a lat)
    (cond((null? lat)#f)
         (else(or(eq?(car lat) a )
                 (menber? a (cdr lat)))))))
(menber? 'tea '(coffee tea or milk))#true 
(menber? 'poached '(fried egges and scrambles eggs))#false


;;

;;conditional

;; If is great for either -or choice
(define(roman-value letter)
  (if(equal? letter 'i)1
     (if(equal? letter 'v)5
        (if(equal? letter 'x)10
           (if(equal? letter 'l)50
              (if(equal? letter 'c)100
                 (if (equal? letter 'd)500
                     (if(equal? letter 'm)1000 'nope?))))))))
(roman-value 'c)
;; equivalent of if in cond
(define (roman-value2 letter)
  (cond((equal? letter 'i)1)
       ((equal? letter 'v)5)
       ((equal? letter 'x)10)
       ((equal? letter 'l)50)
       ((equal? letter 'c)100)
       ((equal? letter 'd)500)
       ((equal? letter 'm)1000)
       (else 'nope)))
(roman-value2 'y)


;; here is a procedure that translate scheme true or false values into more human-readable word true and false

(define(truefalse value)
  (cond(value'true)
        (else 'false)))

(truefalse (= 2(+ 1 1))) 


(define (permit age)
  (if(> age 18) 'yes 'no))
(permit 15)

(define(permit2 age)
  (cond((> age 18) 'yes)
       ((< age 18) 'no)))
(permit2 20)

;; absolute value
(define (myabs x)
  (if( > x 0) x (- x)))
(myabs -5)

  (define myabs2(lambda(x)
                  (cond(( > x 0) x)
                       ((< x 0) (- x))
                       (else(0)))))
 (myabs2 -6)
;; negative number need to be in paranthesis.

;;hw 1

;;2 recursive and iterative sum of digit

(define (sod num)
  (cond((< num 0) 0)
       ((< num 10) num)
       (else(+ (remainder num 10)(sod(quotient num 10))))))
(sod 3456)

;; the remainder of num and 10 give last digit
;; then the quotient with 10 give you the remaining digit
;;recursive call reduce the size of digit then add it to the remainder with 10

;;sum of digit iterative solution
;; invariant is the sum.
;; at every iteration we are accumulating the result so far.
;;pre cond  num is non-negative
;;post condition: the sum of the digit is returned.
(define (sum-iter-helper num sum)
  (cond((<= num 0)sum)
       (else(sum-iter-helper(quotient num 10)(+ sum (remainder num 10))))))
   
  
(define (sumd-iter num)
  (sum-iter-helper num 0))

(sumd-iter 345)

;; function below return the sum of largest 2.
(define (sum-of-lrgtwo x y z)
  (let((lowest (min x y z)))
    (cond((= x lowest)(+(sqrr y )(sqrr z)))
         ((= y lowest)(+(sqrr x )(sqrr z)))
         ((= z lowest)(+(sqrr y )(sqrr x))))))

(sum-of-lrgtwo 1 2 3)

(define (pascal row col)
  (cond((or (= col 0)(>= col row ))1)
       (else(+(pascal(- row 1 )(- col 1))(pascal(- row 1)col)))))
(pascal 4 2)

;;The two values to sum in order to calculate Pascal(row, column)
;;are always going to be the value in the previous row and same column,
;;Pascal(row-1, column), and the value in the previous row and previous column,
;;Pascal(row-1, column-1).

(define (pascal2 row col)
   (cond ((< row col) #f)
         ((or (= 0 col) (= row col)) 1)
         (else (+ (pascal2 (- row 1) col)
                  (pascal2 (- row 1) (- col 1))))))
(pascal2 4 2)
;; proof: we observe that the fun
;To prove that the function works for all elements, we rst assume it works for any arbitrary element k in
;any arbitrary row r. We have already demonstrated that, no matter what value we set for r, the function is
;correct if k happens to be the rst or last element in the row. Beyond that, we must show that, assuming
;pascal r k correctly returns (+ (pascal (- r 1) (- k 1)) (pascal (- r 1) k)), it will also
;return the correct value for r + 1 and k + 1. That is, pascal (+ r 1) (+ k 1) should return the sum
;of pascal r k and pascal r (+ k 1), the sum of the element before k in the row above r and the
;element at position k + 1 in the row above r + 1.

;; A recursive solution to return number digit are in increasing order

(define (rec-increasing-ordr-digit num)
  (cond ((< num 100)(>= (remainder num 10 )(quotient num 10)))
        (else (and (>= (remainder num 10 )(remainder (quotient num 10)10))
                   (rec-increasing-ordr-digit(quotient num 10))))))

(rec-increasing-ordr-digit 1234)



;; A procedure that count the number of word anywhere within a structure list.
;; need to define atom
;; atom is not null and not a pair
(define atom?
  (lambda (x) (and (not(pair? x))(not (null? x)))))


               
(define (deep-count lst)
  (cond(( null? lst) 0)
       ((atom? (car lst))(+ 1(deep-count(cdr lst))))
       (else(+(deep-count (car lst))
              (deep-count (cdr lst))))))

(deep-count '((2 3 4 5) abc a ))
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;First practice end here

;;;;;;;;;;;;;;;;;2nd practice start here
(define atom?(lambda(x)
               (and(not(pair? x))(not(null? x)))))
(atom? 4)


;; a lat an s expression that is a list of atom.
(define lat?
  (lambda (l)
    (cond
      ((null? l)#t)
      ((atom? (car l))(lat?(cdr l)))
    (else #f))))
(lat? '(jack sprat could eat no chicken))#true
(lat? '(jack sprat '(could) eat no chicken))#false


;; menber return true if the argument a exist in the list of atom lat
(define menber?
  (lambda(a lat)
    (cond((null? lat)#f)
         (else(or(eq?(car lat) a )
                 (menber? a (cdr lat)))))))
(menber? 'tea '(coffee tea or milk))#true 
(menber? 'poached '(fried egges and scrambles eggs))#false



;; write a function that remove the first occurance of atom a from a lat.
(define rember
  (lambda(a lat)
    (cond((null? lat)(quote ()))
         ((eq? (car lat) a)(cdr lat))
         (else (cons(car lat)(rember a (cdr lat)))))))
(rember 'mint '(lamb chop and mint jelly))
(rember 'cup '(coffe cup tea cup and hick cup))
(rember 'toast '(bacon lettuce and tomato))
(rember 'and '(bacon lettuce and tomato))
(rember 'sauce '(soy sauce and tomato sauce))
;; first take a list that is either null or contains non-empty element
;;list as argument
;; it built a list compose of first s-eprex of each internal list
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))
(firsts '((apple peach pumpkin) 
          (plum pear cherry) 
          (grape raisin pea) 
          (bean carrot eggplant)))
(firsts '(((five plums) four) (eleven green oranges) ((no) more)))

;; define insertRight take 3 argument new atom old and lat
;; the function build a lat with new atom inserted to the right of first
;; occurance of old

(define insertRight
  (lambda( new old lat )
    (cond((null? lat) '())
         ((eq? (car lat) old)(cons old(cons new (cdr lat))))
         (else(cons (car lat)(insertRight old new (cdr lat)))))))

(insertRight 'topping 'fudge '(ice cream with fudge for dessert))

(define insertLeft
  (lambda( new old lat)
    (cond((null? lat) '())
         ((eq? (car lat) old)(cons new lat));; just add new to the  orginal list
         (else (cons (car lat) (insertLeft old new (cdr lat)))))));;
(insertLeft 'e 'd '(a b c d f g d h))
(insertRight 'e 'd '(a b c d f g d h))  



;;define subst  which replace the first occurence of old in lat with new
(define subst
         (lambda( new old lat)
           (cond((null? lat) '())
                ((eq? (car lat) old)(cons new (cdr lat)))
                (else (cons(car lat)(subst new old (cdr lat)))))))

(define (subst2  new v1 v2 lat)
  (cond((null? lat) '())
       ((or (eq? (car lat) v1)(eq? (car lat) v2)) (cons new (cdr lat)))
       (else (cons( car lat)(subst2 new v1 v2 (cdr lat))))))
(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
;;; function to determine if a is menber of lat( list of atom.) 
(define (menber? a lat)
  (cond((null? lat)#f)
       (else(or (eq? (car lat) a)(menber? a (cdr lAt))))))

(menber? 'meat '(potatoe masshed and gravy MEAT ))


;; proof and correctness:
;; precond:
;a is an atom
;; lat is a list of astom
;;Postcond
;;(menber? a lat)=#t when the atom a occur in the list 

;; noting that the menber? as a recursive form
;; we show that given that the precod hold , we reach the post condition
;; we argue by doing induction the length of lat
;; if lat has length 0, then of course, a does not occur in lat
;; menber? a lat correctly return #f


;;now we assume that the recursive(menber? a (cdr lat)) call work correctly
;; and consider (menber? a lat) if a = (car lat) , (eq? (car lat) a ) is then #t.
;; and the (or ...) return true, this is then the value of the call (menber? a lat)
;; and as a does occur in lat this is cleary correct.


;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; An L-expression is an S-expression which is either:
;   (AND l1 l2), or
;   (OR l1 l2), or
;   (NOT l1), or


; (a) Write and certify a function lexp? which checks whether an S-expression is an L-expression.
(define (atom? x)
  (not (or (pair? x) (null? x))))

(define (lexp? ex)
  (define (operator x)
    (car x))
  (define (first-op x)
    (car (cdr x)))
  (define (second-op x)
    (car (cdr (cdr x))))
  
  (cond ((null? ex) #f)
        ((atom? ex) (if (number? ex) #f
                        #t))
        ((or (eq? (operator ex) 'AND)
             (eq? (operator ex) 'OR)
             (eq? (operator ex) 'NOT)) (and (lexp? (first-op ex))
                                            (lexp? (second-op ex))))
        (else #f)))

; We prove the correctness of lexp? by inducting on the complexity of the arguments. We first note that an atom is any S-expression which is neither a pair
; nor null. This is simply for clarity. In the simplest case, the case in which an L-expression is an atom (or arbitrary symbol, as the definition 
; calls it), the function returns true if the atom is a number or true otherwise, as expected. If the L-expression consists of one of the defined operators and two
; atomic operands, the recursive calls (lexp? first-op ex) and (lexp? second-op ex) will both return true, and thus the value returned will be true, which is the
; correct behavior. We now assume that, for an L-expression of arbitrary complexity, the recursive calls both correctly return whether or not their arguments are 
; valid L-expressions. Regardless of the complexity of the two arguments (i.e. the number of nested L-expressions), the function will always return a simple
; #t or #f value which will be used in the and condition to return whether the expression is valid based on the validity of the two
; operands.

; (b) Write and certify a function covered? of an L-expression lexp and a list of symbols los that tests
;     whether all the variables in lexp are in los.

(define (covered? lexp los)
  (define (member? a ls)
    (cond ((null? ls) #f)
          ((equal? a (car ls)) #t)
          (else (member? a (cdr ls)))))

  ; list of symbols is empty, so none of the variables in lexp are in it
  (cond ((atom? lexp) (member? lexp los))
        ((null? los) #f)
        ; lexp is empty so all symbols in lexp (i.e. none) are in los
        ((null? lexp) #t)
        ; check nested lexpressions for symbols and check if they're in los
        ((pair? (car lexp)) (and (covered? (car lexp) los)
                                 (covered? (cdr lexp) los)))
        ; car must be an atom. if it's an operator, we can skip it since it isn't a symbol
        ((or (equal? (car lexp) 'AND)
             (equal? (car lexp) 'OR)
             (equal? (car lexp) 'NOT)) (covered? (cdr lexp) los))
        ; car must be a symbol. check if it's in los. if not, the answer is false. if so, also check the cdr of lexp
        (else (and (member? (car lexp) los) (covered? (cdr lexp) los)))))

(covered? '(AND (AND l1 l2) l3) '(l1 l2 l3 l4))

; (c) For the evaluation of L-expressions we need association lists, or alists.  An alist for
;     L-expressions is a list of (variable, value) pairs.  The variable component is always a symbol, and
;     the value component is either the number 0 (for false) or 1 (for true). Write and certify a function
;     lookup of the symbol var and the association list al, so that (lookup var al) returns the 
;     value of the first pair in al whose car is eq? to var.

(define (lookup var al)
  ; al is list of (variable value) pairs e.g.
  ; ((l1 0) (l2 1) (l3 1) (l4 0)...)
  ; Improving readability
  (define (variable entry)
    (car entry))
  (define (value entry)
    (car (cdr entry)))

  ; I guess we don't need error checking
  (cond ((eq? (variable (car al)) var) (value (car al)))
        (else (lookup var (cdr al)))))

(lookup 'l2 '((l1 0) (l2 1) (l3 1) (l4 0)))

; (d) If the list of symbols in an alist for L-expressions contains all the variables of an L-expression
;     lexp, then lexp is called _closed_ with respect to this alist.  A closed L-expression can be evaluated,
;     essentially by substituting the values of the variables given in the alist for the variable occurrences
;     in the L-expression.  You are asked to write and certify the function value of an L-expression 
;     lexp and an alist al, which, after verifying that lexp is closed with respect to al,
;     determines whether lexp means true or false.  If lexp is not closed wrt al, then (value lexp al)
;     should return the symbol not-covered.

(define (value lexp al)
  ; readability
  (define (variable entry)
    (car entry))

  (define (operator lexp)
    (car lexp))
  (define (op1 lexp)
    (car (cdr lexp)))
  (define (op2 lexp)
    (car (cdr (cdr lexp))))
  
  ; retrieve list of symbols from al
  (define (get-symbols list)
    (cond ((null? list) '())
          (else (cons (variable (car list)) (get-symbols (cdr list))))))

  ; because (not 0) is #f. Apparently.
  (define (numtotruth num)
    (if (= num 0) #f
        #t))
  
  ; first check if lexp is covered.
  (cond ((not (covered? lexp (get-symbols al))) 'not-covered)
        ; if the expression is just an atom, look it up in the table if it isn't already a value
        ((atom? lexp) (if (number? lexp) (numtotruth lexp)
                          (numtotruth (lookup lexp al))))
        ((eq? (operator lexp) 'AND) (and (value (op1 lexp) al)
                                         (value (op2 lexp) al)))
        ((eq? (operator lexp) 'OR) (or (value (op1 lexp) al)
                                       (value (op2 lexp) al)))
        ((eq? (operator lexp) 'NOT) (not (value (op1 lexp) al)))))

(value '(AND (OR l1 l2) (NOT l3)) '((l1 0) (l2 1) (l3 0))) ; (AND (OR 0 1) (NOT 0)) == (AND 1 1) == 1 == #t
         
;;;;;;;;;;;;;;;abu stuff
(define (permit age)
  (cond ((> age 18) 'yes)
        ((< age 18) "no")
        ((= age 18) 0)))
(permit 18)


(and (= 2 2) (> 2 3))

;; absolute value function

(define myabs
  (lambda (x)
  (cond ((< x 0) (- x))
        (else x))))

(myabs 0)

;; recursive sum of digit
(define (sum-of-digits num)
   (cond ((< num 0) #f)
         ((< num 10) num)
         (else (+ (remainder num 10) (sum-of-digits (quotient num 10))))))

(sum-of-digits 345)

;; iterative sum of digit
(define (sodRec num)
  (define (sodRec1 num sum)
    (cond ((<= num 0) sum)
   (else (sodRec1 (quotient num 10) (+ sum (remainder num 10))))))
  (sodRec1 num 0))

  (sodRec 3451)

 ;; member function

 (define member?
   (lambda (a lat)
   (cond ((null? lat) #f)
         ((eq? a (car lat)) #t)
         (else (member? a (cdr lat))))))
 

 (member? 'moh '(abu is my name))

 ;; build function
 (define build
   (lambda (s1 s2)
   (cons s1 (cons s2 (quote ())))))
 (build 'a 'b)


 abu
  

;; show that TLS scheme implement first class function correctly.
;; Prove that Tls correctly implement lexical scope?


;; Exam 1 answer start here;;;;;;;;;;;;;;;;;;

  ; exam1, question 1

;;;ex1 q1 kris
  (define (dss n)
    (define (iter n p q)
      (cond ((< n 1) n)
            ((= p n) n)
            ((= (* q q) (- n (* p p))) p)
            ((= q n)(iter n (+ p 1) 2))
            (else (iter n p (+ q 1)))))
      (iter n 2 2))
  (dss 50)
;;; end ex1 q1 kris
;;;ex1 q1 chinese
(define (square x) (* x x))
(define (dss n)
    (define (dss-iter guess-p guess-n-p)
      (cond ((>= guess-p (/ n 2)) n)
            ((= guess-n-p n) (dss-iter (+ guess-p 1) (+ guess-p 1)))
            ((= (+ (square guess-p) (square guess-n-p)) n) guess-p)
            (else (dss-iter guess-p (+ guess-n-p 1)))))
    (dss-iter 2 2))
;;; end ex1 q1 chinese

;;exam 1 question 2

(define (numlen num)
  (cond((< num 10)1)
       (else(+ 1(numlen(quotient num 10))))))

(define (left-part num  expo)(quotient num (expt 10 expo)))
(define (right-part num  expo)(remainder num (expt 10 expo)))
 (numlen 1234)

(define(sum num)
  (cond((< num 10) num)
       (else(+(sum(left-part num (quotient(numlen num) 2)))(sum(right-part num (quotient(numlen num) 2)))))))

 
(left-part 1234  2)
(right-part 1234  2)

(quotient 123456 100)
(remainder 123456 1000)
(sum 12345)
;;;;;;;;;;;;;;;;;;;;;;;;end with ema
  ;exam1, question 3
  ; iterative
  (define (log-approx n)
    (lambda (x)
      (define (iter count result-so-far)
      (cond ((= count n) result-so-far)
            (else (iter (+ count 1) (+ result-so-far (/ (expt x count)count))))))
      (iter 1 0 )))

  ((log-approx 100) 0.75)

   
 ; GCD
  (define (gcd1 a b)
  (if (= b 0)
      a
      (gcd1 b (modulo a b))))

  (gcd1 2 5)


;;;;;;;;;; 2nd exam question start here
(define (precedes id1 id2 lst)
      (cond ((eq? id1 (car lst)) #t)
           ((eq? id2 (car lst)) #f)
           (else (precedes id1 id2 (cdr lst)))))

(define (precedes id1 id2 lst)
  (if(null? lst)#f
  (if(eq? id1 (car lst))#t
     (if(eq? id2 (carlst))#f (precedes id1 id2 (cdr lst))))))
        
;pre-condition: (1)lst is not empty.
;               (2)num1 and atom2 both are from the lst, and atom1 and atom2 is different.
;post-condition: one of atom1 and atom2 first occurs in lst. if atom1 occurs first then #t, otherwise #f.


 ;;;  exam2 question 2 fpip start

 (define varlist '(a b c d e f g))

 (define (in-varlist? x varlist)
   (cond ((null? varlist) #f)
         ((eq? x (car varlist)) #t)
         (else (in-varlist? x (cdr varlist)))))

 ;(in-varlist? 'm varlist)

 (define (atom? x)
     (not (or (null? x) (pair? x))))

 (define (length1 exp)
     (cond ((null? exp) 0)
           ((atom? exp) 1)
           (else (+ 1 (length1 (cdr exp))))))

 ;(length1 '(a b c d))

 (define (valid-length? exp)
   (if (> (length1 exp) 3) #f #t))
 ;(valid-length? '(a b c))

 
 (define (operator x)
   (car (cdr x)))
 (define (first-operand x)
   (car x))
  (define (second-operand x)
    (car (cdr (cdr x))))

  ;(operator '(a + b))
  ;(first-operand '(a + b))
  ;(second-operand '(a + b))


  (define (checker? exp)
    (cond ((null? exp) #f)
          ((atom? exp) (if (in-varlist? exp varlist) #t #f))
          ((and (valid-length? exp) (eq? (operator exp) '+)) (and (checker? (first-operand exp)) (checker? (second-operand exp))))))
           
  (checker? '(a - (b + c)))
;;;;;; ;;; ;; exam2 question 2 fpip end Me and abu


;;Utility functions exam 2 prajwal start;;;;;;;;;;;;;;;;;;;;;;

(define atom?
  (lambda (exp)
    (and (not (null? exp))(not (pair? exp)))
  )
)

(define lat?
  (lambda (lat)
    (cond  ((null? lat) #t)
           ((list? (car lat)) #f)
           (else (lat? (cdr lat)))
    )
  )
)

(define first-operand
  (lambda(lst)
    (cond ((atom? lst) lst)
      (else (car lst) ))
  )
)

(define second-operand
  (lambda(lst)
    (car ( cddr lst) )
  )
)

(define operator
  (lambda (lst)
    (cadr lst)
  )
)

(define operator?
  (lambda (exp)
    (or (eq? exp `+) (eq? exp `*) (eq? exp `-) (eq? exp `/))
  )
)


(define order-lat
  (lambda (lat)
    (cond ((< (first-operand lat) (second-operand lat)) lat)
          (else (list (second-operand lat) '+ (first-operand lat) ))
    )
  )
)

(define insert-fpip
  (lambda (elem fpip)
    (cond((< elem (car fpip))(cons elem (list '+ fpip)))
         ((lat? fpip)(cond ((< elem (first-operand fpip)) (list elem '+ fpip))
                           ((< elem (second-operand fpip)) (list (first-operand fpip) '+ (list elem '+ (second-operand fpip))))
                           (else (list (first-operand fpip) '+ (list (second-operand fpip) '+ elem)))
                     )
         )
         (else (list (first-operand fpip) '+ (insert-fpip elem (second-operand fpip))) )
    )
  )
)

(define combine-two-list
  (lambda (l1 l2)
    (cond((atom? l1)(insert-fpip l1 l2))
         ((atom? l2)(insert-fpip l2 l1))
         (else (combine-two-list (second-operand l1) (insert-fpip (first-operand l1) l2)))
    )
    )
  )

(define fasl
  (lambda (lst)
    (cond ((and (atom? (first-operand lst))(list? (second-operand lst))) #t)
          (else #f)
    )
  )
)
(define flsa
  (lambda (lst)
    (cond ((and (list? (first-operand lst))(atom? (second-operand lst))) #t)
          (else #f)
    )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;Qno1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define preceeds?
 (lambda( x y lst)
   (cond ((eq? (car lst) x) #t)
         ((eq? (car lst) y) #f)
         (else (preceeds? x y (cdr lst)))
   )
   
 )
)

;;;;;;;;;;;;;;;;;;;;; Qno2;;;;;;;;;;;;;;;;;;;;;
;(de;fine check
  ;(;lambda (i-aexp)
    ;(cond((atom? i-aexp) #t)
     ;    ((or (not (eq? 3 (length i-aexp))) (not (operator? (operator i-aexp))) (null? (first-operand i-aexp)) (null? (second-operand i-aexp))) #f)
      ;   ((and (atom? (first-operand i-aexp))(atom? (second-operand i-aexp))) #t)
       ;  ((atom? (first-operand i-aexp))(check (second-operand i-aexp)))
        ; ((atom? (second-operand i-aexp))(check (first-operand i-aexp)))
         ;(else (and (check (first-operand i-aexp)) (check (second-operand i-aexp))))
    ;)
  ;)
;)

;;;;;;;;;;;;;;;;;;;;;;;;;;Qno3;;;;;;;;;;;;;;;;;;;;;;


(define apply-commutivity
  (lambda(lst)
    (cons (second-operand lst) (cons '+ (cons (first-operand lst) '())))
    )
  )
(apply-commutivity '(b  + a))

(define apply-associativity
  (lambda (lst)
    (cond((atom? (first-operand lst))(list (list (first-operand lst)  '+  (first-operand (second-operand lst))) '+ (second-operand (second-operand lst))))
         (else (list (first-operand (first-operand lst)) '+ (list (second-operand (first-operand lst)) '+ (second-operand lst))))
         
    )
  )
)

(define greater >)
(define smaller <)
(define olie
  (lambda (lst )
    (cond ((and (check-l-fpip lst)(lat? (second-operand lst)) (greater(first-operand(second-operand lst))(second-operand(second-operand lst)))) (olie (list (first-operand lst) '+ (apply-commutivity (second-operand lst)))))
          ((and (check-l-fpip lst)(greater (first-operand lst) (first-operand (second-operand lst)))) (olie (apply-commutivity lst))) 
          ((check-l-fpip lst) lst)
          ((atom? (first-operand lst)) (olie (list (first-operand lst) '+ (olie (second-operand lst))) ))
          ((list? (first-operand lst))(olie (apply-associativity lst))) 
          (else (olie(apply-associativity lst)))
    )

  )
)

(define check-l-fpip
       (lambda(lst) 
         (cond ((null? lst) #t)
               ((atom? lst) #t)
               ((= 1 (length lst)) #t)
               ((atom? (first-operand lst))(check-l-fpip (second-operand lst)))
               (else #f)
         )
       )
)
(olie '(2 + ((((((5 + 7) + 9) + 5) + 3) + ((((8 + 11) + 6) + 9) + 7)) + (((3 + 1) + 9) + 2))) )
;(check-l-fpip '(a + ((b + g) + c)))
;(second-operand '(a + ((b + g) + c)))
;(2 + (4 + (8 + (6 + (4 + (3 + (1 + (11 + (5 + (7 + (9 + (1 + (3 + (4 + 2))))))))))))))
;'(1 + (2 + (2 + (3 + (3 + (5 + (5 + (6 + (7 + (8 + (9 + (9 + (9 + (11 + 7))))))))))))))

;;;;;;;exam 2 prajwal end here

;;; Add  Primitve to TLS:
;; For adding primitive (+, -, / %) first add it in atom-to action
    ((eq? e (quote +)) *const)
;; and add this in my-apply primitve
((eq? name (quote +))
       (+ (first vals) (second vals)))


;; when addin if tls USE this to list-to-action  ((eq? (car e) (quote if)) *if)

(define fourth cadddr)

(define if-to-cond
  (lambda(e)
    (list 'cond (list (second e) (third e)) (list 'else (fourth e)))
  )
)

(define *if
  (lambda (e table)
    ((lambda (associated-cond-exp)
      (meaning associated-cond-exp table)
    )
    (if-to-cond e))
  ))

(value '(if (> 3 4) #t #f))

;;;;; Prove for Lexical Scoping and more example
;;

In Scheme, things are pretty similar. Blocks can be created with let expressions, like so:

...
(let ((x 10)
      (y 20))
   (foo x y))
...
The first part of the let is the variable binding clause, which in this case two subclauses, (x 10) and (y 20). This says that the let will create a variable named x whose initial value is 10, and another variable y whose initial value is 20. A let's variable binding clause can contain any number of clauses, creating any number of let variables. Each subclause is very much like the name and initial value parts of a define form.

The rest of the let is a sequence of expressions, called the let body. The expressions are simply evaluated in order, and the value of the last expression is returned as the value of the whole let expression. (The fact that this value is returned is very handy, and will be important in examples we use later.)

A let may only bind one variable, but it still needs parentheses around the whole variable binding clause, as well as around the (one) subclause for a particular binding. For example:

...
(let ((x 10))
   (foo x))
...
(Don't forget the "extra" parentheses around the one variable binding clause--they're not really extra, because they're what tells Scheme where the variable binding clause starts and stops. In this case, before and after the subclause that defines the one variable.)

In Scheme, you can use local variables pretty much the way you do in most languages. When you enter a let expression, the let variables will be bound and initialized with values. When you exit the let expression, those bindings will disappear.


;; Strutural induction on TLS start here
;; adding let to the system.

;; extend TLS by adding let.
;; there is no bound on Let.( meaning we can have nested )
;;meaning the new system must be able able (let( x 1)(y 2))
;; (cond((=x 1 )(let((w3)))))

;; the main ideas is: Add a new type *let, cond, define  the corresponding action function. *let desugar the outermost let
;; that is (let(x1 v1)-----((xn vn)) let-body)

;;IH if let occured in strutural smaller compoment, then we can asssume that it work and handle let at the top most level.


;; Post- syntax tranformtion is correctly done.





;; show that TLS scheme implement first class function correctly.

;;;-----------------------------------------------------------------lexical scope start
;; Prove that Tls correctly implement lexical scope?


;the overall argument is showing that this goes by structural induction on tls.
;how let consider 2 aspect of lexcial scope
; 1- variable shadowing - correct implementation of local vasriable 
;2- correctly handling of free variable in the creation of closure

; first thing to realise is that most of cases which strutural induction go through
;;vacuously without effect. this is because lexical scoping only arises in correction with application of lamdba forms.

;for example. In the basis step there is nothing to do because such applications do not occur in numbers, primitives, identifiers or thruth value.


;; specialy we want to show that if e is a well defined form TLS expressions, then the interpreter handle 
;;local and free variable  in the closure correctly.
;; it handle  binding correctly with lambda stacking of the environment we use to prove the correctness.

;;Similary most part of the induction step, the conclusion of the correctness  immediately formed the IH.
;; for example  if TLS evaluate a (cond (p1 e1)(p2 e2)...)then any occurance of code give rise to the structural creation of closures.

;1- variable shadowing and 2- Correct handling of free variables, must eventually and entirely contained in one of p1 and one of 2.
;;Specially, we want to show that if 2 is a  well formed TLS expression, then the interpreter handdle 1-2 correctly.
;;;-----------------------------------------------------

;; for canonical IH for structureal induction, the property hold for all strutural smaller components.
;;;;; Look at TLS and give similar for *lambda, Applications of primitive.


;; the only case requiring some work is ((Lambda)(x)g)v) where g is a TLS expression and v is a TLS Value.
;; for this form, we know that g will be evaluated in an environment whose bottom rib(Rightmost rib) is((x),(V))
;; this meam that any lambda form which occur in g are either entirely evaluated  using ribs above this one(perhap created just in time) or the lambda form
;;contains a free occruence of  x .


;; this follow from the design of apply-closure which put new ribs in-front of the old ribs and from the design of environment subsystem- which
;; does lookup starting with the topmost(left most rib).


; ENV ((x)(v)) No code fr removing that rib.
;; When call finish there, it goes back to the original.

;;-- asssume that g is a well formed  TL Sexpression and ALL variable have Bidding..
;; we chech within the first  rib, if not available, we go to the second rib. If not in the second(or inner most rib), we get an error.

;;--IH. if there is  free variable in g that is not x, every -variable  that occur in g other than x have biding(free-variable in closure)

;; local variable top most rib add a new rib at the top of most recent is left to the environment
;; element below  would be earlier and  and showdowed.



;;--------------------------------------------------lexcial scope end.

;;  ;proof  example for lat useful for other list proof

(define (lat? lst)
  (cond((null? lst)#t)
       (else(and(atom? (car lst))(lat?(cdr lst))))))
  ;Basis: if the argument is () the program lat? returns #t. this is correct by the definition of lat
  ;Expected IH: the program works on smaller inputs, as measure by datatype, the program's input belongs to
  ;IS: with the EIH, consider inputs which are not '(), according to Def, there are 2 cases to be considered
  ;either the car is an atom or the car is a list.
  ;if the car is an atom the program returns #t if the (cdr lst) is a lat and if otherwise if (cdr lst) is a list of atom, then
  ;clearly lst = ((cons atom (cdr lst)) is a list of atoms so os the right value to return and if (cdr lst) is not a lst of atom, then #f.
  ;Termination: We are cdring down the list until () is detected. We know () will be detected because the input belongs to. 

(define (lat? lst)
  (cond((null? lst)#t)
       (else(and(atom? (car lst))(lat?(cdr lst))))))




