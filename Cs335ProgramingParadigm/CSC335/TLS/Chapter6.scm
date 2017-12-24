(define (atom? a)
  (not (or (null? a) (pair? a))))

(define (numbered? aexp)
  ; a-expressions can't be null so we don't need to check
  (cond ((atom? aexp) (number? aexp))
        ; an a-expression with only one term might well be a numbered one
        ((null? (cdr aexp)) (number? (car aexp)))
        (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))))

(numbered? '(3 + (4 ^ 5)))
(numbered? '(2 * sausage))
(numbered? 1)
(numbered? '(1)) ; not a valid aexp but we account for it anyway

(newline)

; some helper functions to add a layer of abstraction
(define (first-operand aexp)
  ; 2 in (2 + (3 * 4))
  (car aexp))

(define (second-operand aexp)
  ; (3 * 4) in (2 + (3 * 4))
  (car (cdr (cdr aexp))))

(define (operator aexp)
  ; + in (2 + (3 * 4))
  (car (cdr aexp)))

(define (sum a b)
  (+ a b))

(define (product a b)
  (* a b))

(define (pow a b)
  (expt a b))
; works only on numbered aexp
(define (value aexp)
  (cond ((atom? aexp) aexp)
        ((eq? (operator aexp) '+) (sum (value (first-operand aexp))
                                  (value (second-operand aexp))))
        ((eq? (operator aexp) '*) (product (value (first-operand aexp))
                                           (value (second-operand aexp))))
        ((eq? (operator aexp) '^) (pow (value (first-operand aexp))
                                       (value (second-operand aexp))))))

(value '(2 + (3 * 4))) ; 14
(value '(1 + (3 ^ 4))) ; 82

(newline)
; did we just write a shitty little interpreter for arithmetic expressions
; good lord

; we don't need to touch the value function to change the implementation
; we can just change these to switch to prefix notation, for instance
(define (first-operand aexp)
  ; 2 in (+ 2 (* 3 4))
  (car (cdr aexp)))

(define (second-operand aexp)
  ; (3 * 4) in (+ 2 (* 3 4))
  (car (cdr (cdr aexp))))

(define (operator aexp)
  ; + in (+ 2 (* 3 4))
  (car aexp))

(value '(+ 2 (* 3 4))) ; 14
(value '(+ 1 (^ 3 4))) ; 82
(newline)

(define (sero? a)
  (null? a))

(define (edd1 a)
  (cons '() a))

(define (zub1 a)
  (cdr a))

(sero? '())
(sero? '(() ()))
(edd1 '())
(edd1 '(() () ()))
(zub1 '(() () ()))
(zub1 '(()))
(newline)

(define (edd a b)
  (cond ((sero? b) a)
        (else (edd (edd1 a) (zub1 b)))))

(edd '(() ()) '(()))