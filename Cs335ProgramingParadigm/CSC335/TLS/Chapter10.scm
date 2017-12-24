(define (atom? x)
  (not (or (null? x) (pair? x))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define (first x)
  (car x))
(define (second x)
  (car (cdr x)))
(define (third x)
  (car (cdr (cdr x))))
(define (build a b)
  (list a b))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    ; they must be of same length so we can just
    ; replace this with a single null? thing
    (cond ((or (null? names) (null? values)) (entry-f name))
          ((equal? name (car names)) (car values))
          (else (lookup-in-entry-help name (cdr names)
                                      (cdr values) entry-f)))))

(lookup-in-entry 'entree
                 '((appetizer entree beverage) (food tastes good))
                 (lambda (x) (cdr '())))
; entry is a pair of lists with first list is a set
; two lists must be of equal length

(define extend-table cons)

(define (lookup-in-table name table table-f)
  (cond ((or (null? name) (null? table)) (table-f name))
        (else (lookup-in-entry name (car table)
                               (lambda (name)
                                 (lookup-in-table name
                                                  (cdr table)
                                                  table-f))))))

(lookup-in-table 'entree
                 '(((entree dessert)
                    (spaghetti spumoni))
                   ((appetizer entree beverage)
                    (food tastes good)))
                 (lambda (x) (cdr '())))

(cons 'a (cons 'b (cons 'c '())))

(cons 'car
      (cons (cons 'quote
                  (cons
                   (cons 'a
                         (cons 'b
                               (cons 'c
                                     '())))
                   '()))
            '()))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
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
      (else *identifier))))

(define (list-to-action e)
  (cond ((atom? (car e))
         (cond ((eq? 'lambda (car e)) *lambda)
               ((eq? 'quote (car e)) *quote)
               ((eq? 'cond (car e)) *cond)
               (else *application)))
        (else *application)))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

; e.g. (quote hi) should be hi, so we extract the text
; i.e. whatever comes after the quote, which is the second
; element in the list. Which we can get with (second e)
(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
; runs if we failed to find identifier in table
; throw an error.
(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))
;                     table       formals  body
'(non-primitive ((((y z) ((8) 9))) (x) (cons x y)))
(define (table-of x)
  (first x))
(define (formals-of x)
  (second x))
(define (body-of x)
  (third x))

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
       ((meaning (question-of (car lines)) table)
        (meaning (answer-of (car lines)) table))
       (else (evcon (cdr lines) table)))))

(define (else? x)
  (and (atom? x) (eq? x 'else)))
(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    ; cond (.....)
    (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)

(*cond '(cond (coffee klatsch)
              (else party))
       '(((coffee) (#t))
         ((klatsch party) (5 (6)))))

(define evlis
  (lambda (args table)
    (cond ((null? args) '())
          (else (cons (lookup-in-table (car args)
                                       table
                                       (lambda (x) (cdr '())))
                      (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (myapply (meaning (function-of e) table)
           (evlis (arguments-of e) table))))
(define function-of car)
(define arguments-of cdr)
; (primitive primitive-name)
; (non-primitive (table formals body))

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define myapply
  (lambda (fun vals)
    (cond ((primitive? fun)
           (apply-primitive (second fun) vals))
          ((non-primitive? fun)
           (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons)
       (cons (first vals) (second vals)))
      ((eq? name 'car)
       (car (first vals)))
      ((eq? name 'cdr)
       (cdr (first vals)))
      ((eq? name 'null?)
       (null? (first vals)))
      ((eq? name 'eq?)
       (eq? (first vals) (second vals)))
      ((eq? name 'atom?)
       (:atom? (first vals)))
      ((eq? name 'zero?)
       (zero? (first vals)))
      ((eq? name 'add1)
       (+ (first vals) 1))
      ((eq? name 'sub1)
       (- (first vals) 1))
      ((eq? name 'number?)
       (number? (first vals)))
      ((eq? name 'pair?)
       (pair? (first vals))))))

(define :atom?
  (lambda (x)
    (cond ((atom? x) #t)
          ((null? x) #f)
          ((eq? (car x) 'primitive) #t)
          ((eq? (car x) 'non-primitive) #t)
          (else #f))))

((lambda (x y) (cons x y)) 1 '(2))
; applying a closure to a list of values is same as
; finding meaning of closure's body with its table
; extended by an entry of form (formals values)
; formals is the formals of the closure, values is
; result of evlis

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure) vals)
              (table-of closure)))))
'((((u v w)
    (1 2 3))
   ((x y z)
    (4 5 6)))
  (x y)
  (cons z x))

(meaning '(cons z x)
         '(((x y)
            ((a b c) (d e f)))
           ((u v w)
            (1 2 3))
           ((x y z)
            (4 5 6)))) ; oh jesus christ

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

