; CSc 335
; first scheme interpreter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tls-scheme, from chapter 10 of tls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; need to allow redefinition of initial bindings in r5rs as delivered
; by drracket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; auxiliary functions

(define build
  (lambda (s1 s2)
    ;(display "build = ")(display s1)(display " ")(display s2)(newline)(newline)
    (cons s1 (cons s2 (quote ())))))

(define first car)

(define second cadr)

(define third caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; environments implemented as tables
(define lookup-in-table
  (lambda (name table table-f)
    ;(display "lookup-in-table = ")(display name)(display " ")(display table)
    ;(display " ")(display table-f)(newline)(newline)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

(define extend-table cons)

(define lookup-in-entry
  (lambda (name entry entry-f)
    ;(display "lookup-in-entry = ")(display name)(display " ")(display entry)
    ;(display " ")(display entry-f)(newline)(newline)
    (lookup-in-entry-help name
                          (names entry)
                          (vals entry)
                          entry-f)))



(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    ;(display "lookup-in-entry-help = ")(display name)(display " ")(display names)
    ;(display " ")(display vals)(display " ")(display entry-f)(newline)(newline)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car vals))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr vals)
                                  entry-f)))))


(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define vals
  (lambda (entry) (cadr entry)))


; the top level of the interpreter

;;original
(define value
  (lambda (e)
    ;(display "value = ")(display e)(newline)
    (if (sc e)
        (meaning e (quote () ))
        (display "bad syntax \n"))))

(define (sc e)
  (syntax-checker e '()))

(define (syntax-checker e table)
  (cond ((not (pair? e))
         (atom-checker e table))
        (else (list-checker e table))))

(define (atom-checker e table)
  (cond ((number? e) #t)
        ((boolean? e) #t)
        ((eq? e (quote cons)) #t)
        ((eq? e (quote car)) #t)
        ((eq? e (quote cdr)) #t)
        ((eq? e (quote null?)) #t)
        ((eq? e (quote eq?)) #t)
        ((eq? e (quote atom?)) #t)
        ((eq? e (quote zero?)) #t)
        ((eq? e (quote add1)) #t)
        ((eq? e (quote mul)) #t)
        ((eq? e (quote sub1)) #t)
        ((eq? e (quote number?)) #t)
        ((eq? e (quote ())) #t)
        ((eq? e (quote +)) #t)
        ((eq? e (quote and)) #t)
        ((eq? e (quote or)) #t)
        (else (cover-checker (list e) table))))

(define (list-checker e table)
  (cond ((not (pair? (car e)))
         (cond ((eq? (car e) (quote quote))
                (*quote-checker e table))
               ((eq? (car e) (quote lambda))
                (*lambda-checker (cons table (cdr e))))
               ((eq? (car e) (quote cond))
                (*cond-checker (cdr e) table))
               ((eq? (car e) (quote let))
                #t)
               ((eq? (car e) (quote let*))
                #t)
               ((eq? (car e) (quote if))
                #t)
               (else (*application-checker e table))))
        (else (*application-checker e table))))

(define (*quote-checker e table)
  (= 2 (length e)))

(define (*lambda-checker e)
  (cond ((null? (caddr e))
         #f)
        (else (closure-checker (caddr e) (cons (cadr e) (car e))))))

(define (closure-checker e table)
  (cond ((not (pair? e))
         (cover-checker (list e) table))
        (else (syntax-checker e table))))

(define (cover-checker e table)
  (cond ((null? e) #t)
        ((not (pair? (car e)))
         (cond ((number? (car e)) (cover-checker (cdr e) table))
               ((boolean? (car e)) (cover-checker (cdr e) table))
               (else (member? (car e) (fringe table)))))
        (else (and (cover-checker (car e) table)
                   (cover-checker (cdr e) table)))))

(define (member? e table)
  (if (member e table) #t #f))

(define (fringe table)
  (cond ((null? table) '())
        ((not (pair? table)) (list table))
        (else (append (fringe (car table))
                      (fringe (cdr table))))))

(define (*cond-checker e table)
  (cond ((null? e) #t)
        ((null? (cdr (car e))) #f)
        ((eq? (quote else) (car (car e)))
         (syntax-checker (cadr (car e)) table))
        (else (and (if (syntax-checker (car (car e)) table)
                       (syntax-checker (cadr (car e)) table))
                   (*cond-checker (cdr e) table)))))

(define (*application-checker e table)
  (cond ((or (eq? (car e) (quote cons))
             (eq? (car e) (quote eq?))
             (eq? (car e) (quote mul)))
         (and (= 2 (length (cdr e)))
              (syntax-checker (cdr e) table)))
        ((or (eq? (car e) (quote car))
             (eq? (car e) (quote cdr))
             (eq? (car e) (quote null?))
             (eq? (car e) (quote atom?))
             (eq? (car e) (quote zero?))
             (eq? (car e) (quote add1))
             (eq? (car e) (quote sub1))
             (eq? (car e) (quote number?)))
         (and (= 1 (length (cdr e)))
              (syntax-checker (cdr e) table)))
        (else (and (syntax-checker (car e) table)
                   (syntax-checker (cdr e) table)))))


(define meaning
  (lambda (e table)
    ;(display "meaning = ")(display e)(display " ")(display table)(newline)(newline)
    ((expression-to-action e) e table)))


; supporting functions for the intepeter

; syntax-directed dispatch on expression

(define expression-to-action
  (lambda (e)
    ;(display "expression-to-action = ")(display e)(newline)(newline)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    ;(display "atom-to-action = ")(display e)(newline)(newline)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote plus)) *const)
      ((eq? e (quote +)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote mul)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      ((eq? e (quote and)) *const)
      ((eq? e (quote or)) *const)
      (else *identifier))))


(define list-to-action
  (lambda (e)
    ;(display "list-to-action = ")(display e)(newline)(newline)
    (cond
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         ;; modified for let and let*
         ((eq? (car e) (quote let))
          *let)
         ((eq? (car e) (quote let*))
          **let)
         ;; modified for if
         ((eq? (car e) (quote if))
          *if)
         (else *application)))
      (else *application))))


; operational semantics -- the definitions of the action functions

(define *const
  (lambda (e table)
    ;(display "*const = ")(display e)(display " ")(display table)(newline)(newline)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))


(define *quote
  (lambda (e table)
    ;(display "*quote = ")(display e)(display " ")(display table)(newline)(newline)
    (text-of e)))

(define text-of second)


(define *identifier
  (lambda (e table)
    ;(display "*identifier = ")(display e)(display " ")(display table)(newline)(newline)
    (lookup-in-table e table initial-table)))


; note that as (car (quote ())) throws an error, this definition
; amounts to saying that looking anything up in the initial table
; is impossible.
(define initial-table
  (lambda (name)
    ;(display "*initial-table = ")(display name)(newline)(newline)
    (car (quote ()))))


(define *lambda
  (lambda (e table)
    ;(display "*lambda = ")(display e)(display " ")(display table)(newline)(newline)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)


; cond is a special form that takes any number of 
; cond-lines ...  if it sees an else-line, it treats
; that cond-line as if its question part were true.

(define evcon
  (lambda (lines table)
    ;(display "evcon = ")(display lines)(display " ")(display table)(newline)(newline)
    (cond 
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))


(define else?
  (lambda (x)
    ;(display "else? = ")(display x)(newline)(newline)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)

(define *cond 
  (lambda (e table)
    ;(display "*cond = ")(display e)(display " ")(display table)(newline)(newline)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    ;(display "evlis = ")(display args)(display " ")(display table)(newline)(newline)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    ;(display "*application = ")(display e)(display " ")(display table)(newline)(newline)
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (l)
    ;(display "primitive? = ")(display l)(newline)(newline)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    ;(display "non-primitive? = ")(display l)(newline)(newline)
    (eq? (first l) (quote non-primitive))))

(define myapply
  (lambda (fun vals)
    ;(display "myapply = ")(display fun)(display " ")(display vals)(newline)(newline)
    (cond
      ((primitive? fun)
       (myapply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (myapply-closure
        (second fun) vals)))))

(define (recursive-plus num)
  ;(display num)(newline)
  (cond ((null? num) 0)
        ((atom? (car num))
         (+ (car num) (recursive-plus (cdr num))))
        (else (+ (recursive-plus (car num)) (recursive-plus (cdr num))))))

(define myapply-primitive
  (lambda (name vals)
    ;(display "myapply-primitive = ")(display name)(display " ")(display vals)(newline)(newline)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote and))
       (and (first vals) (second vals)))
      ((eq? name (quote +))
       (recursive-plus vals))
      ((eq? name (quote or))
       (or (first vals) (second vals)))
      ((eq? name (quote add1))
       ((lambda (x) (+ x 1)) (first vals)))
      ((eq? name (quote mul))
       (* (first vals) (second vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))


(define :atom?
  (lambda (x)
    ;(display ":atom? = ")(display x)(newline)(newline)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))

(define myapply-closure
  (lambda (closure vals)
    ;(display "myapply-closure = ")(display closure)(display " | ")(display vals)(newline)(newline)
    (cond ((= (length (formals-of closure)) (length vals)) 
           (meaning (body-of closure)
                                (extend-table
                                 (new-entry
                                  (formals-of closure)
                                  vals)
                                 (table-of closure))))
          (else (display "unbound lambda variable \n")))))

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (*let e table)
  (define element (map car (cadr e)))
  (define body (caddr e))
  (define value (map cadr (cadr e)))
  (meaning (cons (list 'lambda element body)
                 value)
           table))

(define (**let e table)
  (define element (map car (cadr e)))
  (define body (caddr e))
  (define value (map cadr (cadr e)))
  (define (convert-to-let elements values first-time)
    (cond ((null? elements) (list body))
          ((eq? first-time #t)
           (append (list 'let (list (list (car elements) (car values))))
                   (convert-to-let (cdr elements) (cdr values) #f)))
          (else (list (append (list 'let (list (list (car elements) (car values))))
                              (convert-to-let (cdr elements) (cdr values) #f))))))
  (meaning (convert-to-let element value #t) table))

(define (test e table)
  (meaning (first (cdr e)) table))

(define (then-exp e table)
  (meaning (second (cdr e)) table))

(define (else-exp e table)
  (meaning (third (cdr e)) table))

(define (*if e table)
  (if (test e table)
      (then-exp e table)
      (else-exp e table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To this I added *let, **let, *if
; also a syntax-checker
; see syntax-checker.scm for just the syntax checker

;;;

(display "\n Testing small cases \n")

(value '(+ 2 3))

(value '(+ 2))

(value '(cons 2 3))

(value '(cons 2 ))

(value '(cons))

(value '(quote quote))

(value '(dne 2))

(value '(if #t 1 (/ 1 0)))

(value '(if #f (/ 1 0) 1))

(value '(and #t #f))

(display "\n Testing lambda bounds \n")

(value '((lambda (x y)
           (cons x y))
         1 2 3))

(value '((lambda (x y)
           (cons x y))
         1))

(value '((lambda (x y)
           (cons x y))
         1 2))

(value '((lambda (x)
           (number? ))
         2))

(display "\n Testing let \n")

(define sample-let-exp-1
  (quote (let ((x 2) (y 3))
           (cons x y))))

(define sample-let-exp-2
  (quote (let ((x 2) (y 3))
           (let ((w 4) (x 5))
             (cons w (cons x y))))))

(value sample-let-exp-1)

(value sample-let-exp-2)

(define sample-let-exp-3
  (quote (let ((x 2) (y 3))
           (+ x y))))

(value sample-let-exp-3)


(display "\n Testing let* \n")
(define sample-let-exp-4
  (quote (let* ((a 1)
                (b (+ a 2)))
           (+ a b))))

(value sample-let-exp-4)

(define sample-let-exp-7
  (quote (let* ((a 1)
                (b (+ a 2))
                (c 3)
                (d (+ c 2))
                (e 2))
           (+ a b c d e))))

(value sample-let-exp-7)

(define sample-let-exp-8
  (quote (let* ((a 1)
                (b (+ a 2))
                (c 3)
                (d (+ c 2))
                (e 2))
           (+ ))))

(value sample-let-exp-8)

