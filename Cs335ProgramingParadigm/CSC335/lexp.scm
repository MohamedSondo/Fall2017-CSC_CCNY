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