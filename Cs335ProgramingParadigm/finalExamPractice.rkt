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
  
;;
;;
;;
;; 
;;
;;

;;
;;
;;


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



;; lecture eleven 12, 13

;; write a certify lexp which check wether s-expression is an l-expression
;l expression is an s-expr which is either
;( And l1 l2 ), or ( Or  l1 l2 ),
; (NOT l1 )



(define (lexp? ex)
  (define (operator x)
    (car x ))
  (define (first-op x)
    (car(cdr x)))
  (define(second-op x)
    (car(cdr(cdr x))))
  (cond((null? ex)#f)
       ((atom? ex)
        (if (number? ex)#f #t))
       ((or (eq? (operator ex ) 'AND)(eq? (operator ex) 'OR)(eq? (operator ex) 'NOT))
        (and(lexp?(first-op ex))(lexp? (second-op ex ))))
       (else #f)))


;Question 3


(define (order-left-norma ex)
  (define (operator x)
    (car x ))
  (define (first-op x)
    (car(cdr x)))
  (define(second-op x)
    (car(cdr(cdr x))))
  (cond((null? ex)#f)
       ((atom? ex)
        (if (number? ex)#f #t))
       ((or (eq? (operator ex ) 'AND)
            (eq? (operator ex) 'OR)
            (eq? (operator ex) 'NOT)) (and(lexp?(first-op ex))(lexp? (second-op ex ))))
       (else #f)))



;t an atom is any S-expression which is neither a pair
; nor null.
;, the case in which an L-expression is an atom (or arbitrary symbol, as the definition 
; calls it),
;the function returns true if the atom is a number or true .
;If the L-expression consists of one of the defined operators and two
; operands, wthe  recursive calls (lexp? first-op ex) and (lexp? second-op ex) return true,
; for an L-expression of arbitrary complexity, the recursive calls both correctly return whether or not their arguments are 
; valid L-expressions. the function will always return a simple
; #t or #f value which will be used in the and condition to return whether the expression is valid based on the validity of the two
; operands.


;;(greet ’(brian epstein))
;(PLEASED TO MEET YOU BRIAN -- HOW ARE YOU?)
; (greet ’(professor donald knuth))
;(PLEASED TO MEET YOU PROFESSOR KNUTH -- HOW ARE YOU?)

;; functon that great people base of on thier status.
(define (greet name)
  (cond ((null? name) '())
        ((eq? (car name) 'professor)
         (append '(PLEASED TO MEET YOU PROFESSOR) (cons 
                 (car(cdr (cdr name))) '(HOW ARE YOU?))))
        (else (append '(PLEASED TO MEET YOU)
                  (cons (car name) '(HOW ARE YOU?))))))
(greet '(ABU BUTT))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


