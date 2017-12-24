

; CSc 335
; Fall 2016

; November 3

; Second 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: Nelson Batista

; TYPE YOUR FULL EMAIL ADDRESS HERE: nbatist000@citymail.cuny.edu
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1a - code (max 10 points)   6

;;;; Problem 1b - proof (max 10 points)      9

;;;; Problem 1c - code (max 10 points)   10

;;;; Problem 1d - proof (max 10 points)      9

;;;; Problem 1e - code (max 10 points)   5

;;;; Problem 1f - proof (max 10 points)      na



;;;; Problem 2a - code (max 10 points)   9

;;;; Problem 2b - code (max 15 points)   10

;;;; Problem 2c - proof (max 15 points)     na


; Total Code Score (max 55 points)  40

; Total Proof Score (max 45 points)     18

; Total Score (max 100 points)   58

;;;; Letter Grade   C+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using the R5RS
; implementation provided by drracket and only those language features discussed so far in the
; context of lectures and homework.

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones and smart watches and smart rings and ... are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Here are the examination problems.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note that you will need to furnish specifications for your functions before you can prove them correct.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1a.  (10 points) Write a pair of mutually recursive programs odd-indexed-elements and even-indexed-elements to produce from one list L
; the two lists consisting, respectively, of the elements of L with even index and the elements of L with odd index.  For example,
; (odd-indexed-elements '(a b c d e f g h)) = '(b d f h) and (even-indexed-elements '(a b c d e f g h)) = '(a c e g)

(define (odd-indexed-elements l)
  (cond ((null? l) l)
        ((null? (cdr l)) '())
        ((null? (cdr (cdr l))) (cons (car (cdr l)) '()))
        ((null? (cdr (cdr (cdr l)))) (cons (car (cdr l)) '()))
        (else (cons (car (cdr l)) (even-indexed-elements (cdr (cdr (cdr l))))))))

(define (even-indexed-elements l)
  (cond ((null? l) l)
        ((null? (cdr l)) (cons (car l) '()))
        (else (cons (car l) (odd-indexed-elements (cdr l))))))













;(define (odd-indexed-elements L)
;  (cond ((or (null? L) (null? (cdr L))) '())
;        ((null? (cdr (cdr L))) (cons (car (cdr L)) '()))
;        (else (cons (car (cdr L)) (even-indexed-elements (cdr (cdr (cdr L))))))))
;
;(define (even-indexed-elements L)
;  (cond ((null? L) '())
;        (else (cons (car L) (odd-indexed-elements (cdr L))))))

(odd-indexed-elements '(a b))
(odd-indexed-elements '(a b c d e f g h))
(even-indexed-elements '(a b c d e f g h))

;(define (odd-indexed-elements L)
;  (cond ((null? L) '())
;        ((null? (cdr L)) '())
;        (else (cons (car (cdr L)) (odd-indexed-elements (cdr (cdr L)))))))
;
;(define (even-indexed-elements L)
;  (cond ((null? L) '())
;        ((null? (cdr L)) L)
;        (else (cons (car L) (even-indexed-elements (cdr (cdr L)))))))
;;;; these are not the mutually recursive functions I asked for

;;;; otherwise good




; 1b.  (10 points) Prove that both functions are correct (Hint: use one proof for both function).

; We first note that, if the list is a null list, a null list is returned, since the list consisting of elements in a null list with ANY index is
; itself an empty list. Thus the functions return the correct value for the case of an empty list. If the list contains a single element,
; (odd-numbered-elements L) will return an empty list, since that element has index 0, and (even-numbered-elements L) will return the list itself,
; for the same reason.

; Having shown the functions work for the case of (length L) being 0 or 1, we assume that the recursive call (odd-indexed-elements (cdr (cdr L)))
; returns the list of odd-numbered elements of L.  We see that, in this case, assuming L has an arbitrary length of k, a list with length k+1 would
; have its second element (the car of the cdr) with index 1, cons'd to the result of applying odd-indexed-elements to the cdr of its cdr, which is a
; list of length-2 consisting of the list starting at index 2 of the list. Thus, the result is a list consisting of only every OTHER element of the list,
; starting at index 1, which is exactly the list of odd-indexed-elements of the list.

; even-indexed-elements is proved indentically, except we can see that it begins at index 0 rather than index 1 by having (car L) rather than
; (car (cdr L)) in its recursive call. This returns every other element of the list starting at index 0, which, once again, is exactly the list of
; even-indexed-elements of the list.


;;;; nice job of fusing arguments for two separate functions

;;;; you omitted the termination argument




; 1c.  (10 points)  Write a procedure merge which takes two lists of numbers, with each list sorted in order from smallest to largest, which
; produces a third list that consists of all the (distinct) elements of the original lists and is also in sorted order. 

(define (merge l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((= (car l1) (car l2)) (cons (car l1) (merge (cdr l1) (cdr l2))))
        ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
        (else (cons (car l2) (merge l1 (cdr l2))))))

;(define (merge l1 l2)
;  (cond ((null? l1) l2)
;        ((null? l2) l1)
;        ((= (car l1) (car l2)) (cons (car l1) (merge (cdr l1) (cdr l2))))
;        ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
;        (else (cons (car l2) (merge l1 (cdr l2))))))

(merge '(1 3 5 7 9) '(2 4 6 8 10))











(define (merge l1 l2)
  
  (cond ((null? l2) l1)
        ((null? l1) l2)
        ((= (car l1) (car l2)) (cons (car l1) (merge (cdr l1) (cdr l2))))
        ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
        ((> (car l1) (car l2)) (cons (car l2) (merge l1 (cdr l2))))))


;;;; nice

; 1d.  (10 points) Prove that your merge procedure is correct.

; We first assume that l1 and l2 do not contain repeated elements. For example, (1 2 2 3 4) would not be valid as one of the inputs to the function.
; We note that, in the simplest case, where both lists are null, the function returns a null list. If l2 is null, l1 (which is null) will be returned,
; and vice versa. This is correct, as there are no distinct elements in either list, so their merge would also be empty
; If either list is null, the other list is returned. This is correct behavior, as, according to our specification, each list contains distinct numbers
; in sorted order. Combined with a list containing no elements, the merge produces the list itself.

; We will induct on the length of the input lists. Having proved the function works for length = 0,
; we now assume that the recursive call (merge x y) returns a list consisting of the distinct elements in x and y in sorted order.

; With that assumption, we observe that, if the car of x and y are both equal, the car of l1 is cons'd onto the result of (merge (cdr x) (cdr y)),
; so as to prevent duplicate entries in the return value. (cdr lst) returns a list with a length of (length L)-1. 
; If the car of x is greater than that of y, then it is cons'd onto the result of (merge (cdr x) y); that is, the result of applying merge to the
; remainder of x and the list y. Otherwise, we do the same but using the remainder of y and the list x. With this and the knowledge that both lists
; are sorted, eventually one list will become null (since applying the cdr to a list reduces its length by 1), at which point we return the other list.
; since, at each call, we check whether the car of the first list is greater than, less than, or equal to that of the other list, we never have (time is up)

;;;; very good start on the argument 


; 1e.  (10 points) Using your procedures for 1a and 1c, write a procedure merge-sort which inputs a single (unsorted) list of numbers and returns the list
; consisting of the elements of the input list, in sorted order.


(define (merge-sort L)
  ; extract odd and even indexed elements from list, recursively sort, then merge
  (cond ((or (null? L) (null? (cdr L))) L)
        (else (merge (merge-sort (even-indexed-elements L)) (merge-sort (odd-indexed-elements L))))))

;(define (merge-sort L)
;
;  ; we extract the odd and even indexed elements from the list, sort the two, then merge them
;  (cond ((or (null? L) (null? (cdr L))) L)
;        (else (merge (merge-sort (even-indexed-elements L)) (merge-sort (odd-indexed-elements L))))))

(merge-sort '(10 4 9 3 2 5 7 1 8 6))

;;;; you need to call merge-sort recursively on the sublists; you also need to worry about the case when L has just one element.






; 1f.  (10 points)  Prove that your merge-sort procedure is correct.
; our specification requires that the list L not contain any duplicate entries.
; In the case that the list is of length 0, the function simply returns the null list, which is correctly sorted.

; 2a.  (10 points) Write a deeply recursive version of the generalized member? function, that is, your function deep-member? ought to take both a predicate
; and a tree as arguments, and return #t if the tree contains a subtree for which the predicate is true, and #f otherwise.  For example,

; (deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))

; should return #t.  NOTE that I am not requesting a proof for this function.

(define (deep-member? predicate tree)

  (define (atom? x)
    (not (or (null? x) (pair? x))))
  
  (cond ((or (atom? tree) (null? tree)) (predicate tree))
        
        ((atom? (car tree)) (or (predicate tree)
                                (predicate (car tree))
                                (predicate (cdr tree))
                                (deep-member? predicate (cdr tree))))
        
        (else (or (predicate tree)
                  (predicate (car tree))
                  (predicate (cdr tree))
                  (deep-member? predicate (car tree))
                  (deep-member? predicate (cdr tree))))))

;(define (deep-member? predicate tree)
;  (define (atom? x)
;    (not (or (null? x) (pair? x))))
;  
;  (cond ((null? tree) (predicate tree))
;        ((atom? tree) (predicate tree))
;        (else (or (predicate (car tree))
;                  (deep-member? predicate (car tree))
;                  (deep-member? predicate (cdr tree))))))

(deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))

;(define (deep-member? predicate tree)
;  (cond ((null? tree) (predicate tree)) ;;;; good
;
;        ;;;; the only cases you are missing:  (atom? tree) and tree itself
;        
;        ((not (pair? (car tree))) (or (predicate (car tree)) (deep-member? predicate (cdr tree))))
;        ; test the predicate on both subtrees then recur down them in case they contain subtrees which might satisfy
;        (else (or (predicate (car tree)) (predicate (cdr tree))
;                  (deep-member? predicate (car tree))
;                  (deep-member? predicate (cdr tree))))))
;
;(deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))





; 2b.  (15 points) Write a procedure remove-left-most which inputs a predicate and a tree, and which removes only the left-most subtree
; of the input tree for which the predicate is true.  For example,

; (remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e)))))))
;
; =
;
; '((a (b c)) ((f (g (c (d e))))))

(define (remove-left-most predicate tree)

  (cond ((null? tree) '())
        ((predicate (car tree)) (cdr tree))
        ((deep-member? predicate (car tree)) (cons (remove-left-most predicate (car tree))
                                                   (cdr tree)))
        (else (cons (car tree) (remove-left-most predicate (cdr tree))))))

;(define (remove-left-most predicate tree)
;  (define (atom? x)
;    (not (or (null? x) (pair? x))))
;
;  (cond ((null? tree) '())
;        ((atom? tree) (cond ((predicate tree) '())
;                            (else tree)))
;        ; tree is indeed a pair. check if car contains an element satisfying predicate or if it itself satisfies
;        (else (cond ((predicate (car tree)) (cdr tree))
;                    ((deep-member? predicate (car tree)) (cons (remove-left-most predicate (car tree)) (cdr tree)))
;                    (else (cons (car tree) (remove-left-most predicate (cdr tree))))))))

(remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e)))))))


;(define (remove-left-most predicate tree)
;  (cond ((null? tree) '())
;        ; if the car contains a subtree satisfying the predicate, remove it and return the remainder of the list
;
;        ;;;; I assume you did not have a chance to complete this -- notice that you have not yet guaranteed that tree is a pair
;        
;        ((deep-member? predicate (car tree)) (cdr tree))
;        ; otherwise, apply the function to the remainder of the list without changing the car
;        (else (cons (car tree) (remove-left-most predicate (cdr tree))))))
;
;;;;; still, your work is very much in the right direction
;
;
;(remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e)))))))
; Hint: use your deep-member? function.

; 2c.  (15 points) Prove that your remove-left-most procedure is correct. 














