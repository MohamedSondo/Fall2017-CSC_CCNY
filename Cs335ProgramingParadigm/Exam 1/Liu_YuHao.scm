

; CSc 335
; Spring 2017

; May 2

; Second 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: Yu Hao Liu

; TYPE YOUR FULL EMAIL ADDRESS HERE: liuyuhao717@gmail.com / yliu006@citymail.cuny.edu
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (max 7 points)   6
;;;; Problem 1 - proof (max 7 points)    6
;;;; Problem 1 - synergy between proof and code (max 6 points)  5

;;;; Problem 2 - code (max 7 points)  7
;;;; Problem 2 - proof (max 7 points)  5
;;;; Problem 2 - synergy between proof and code (max 6 points)  5

;;;; Problem 3a - code (max 10 points)  10
;;;; Problem 3b - code (max 10 points)   8

;;;; Problem 4 - code (max 15 points)   10
;;;; Problem 4 - proof (max 15 points)    10
;;;; Problem 4 - synergy between proof and code (max 10 points)  5


;;;; Total  17 + 17 + 18 + 25 = 77
;;;; Letter Grade  B+

;;;; good work!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using the implementation of R5RS
; provided by drracket.  ALL FUNCTIONS ARE TO BE GIVEN IN PURE FUNCTIONAL SCHEME.  YOU SHOULD NOT USE STRINGS OR
; VECTORS -- lists will suffice.  

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.
 
; BE SURE TO SAVE YOUR WORK FREQUENTLY.

; DO NOT ERASE EITHER THE QUESTIONS OR THE SCORING RUBRIC, ABOVE.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.



; Problem 1  (20 points)

; Background: Let us say that one list l1 of numbers is smaller than another list l2 of numbers
; if the largest element of l1 is less than the largest element of l2.  Thus '(1 12 15) is smaller than '(17)

; The Problem: Design and prove a predicate list-less-than? which implements this relation.  You should use an iterative
; auxilliary procedure list-max which inputs a list of numbers and which returns the largest element of that
; list.

; Give a proof of your iterative procedure list-max, and make use of this to show that your predicate
; list-less-than? is correct. 
(define (list-max lst)
  (define (process max lst)
    (cond ((null? lst) max)
          ((> (car lst) max) (process (car lst) (cdr lst)))
          (else (process max (cdr lst)))))
  (process 0 lst))   ;;;; you cannot assume 0 is the max -- what if all elements in the list are negative?



(list-max '(1 2 3))
(list-max '(3 2 1))
(define (list-less-than? lst1 lst2)
  (cond ((>= (list-max lst1) (list-max lst2)) #f)
        (else #t)))

(list-less-than? '(1 2 3) '(2 3 4))
;proof
;The guess invariant of list-max is The largest number of LST equals the largest number between the largest number in lst and Max

;;;; good


;when lst is a null, therefore, it will return 0(max) which for guess invariant 0 > null correct.


;;;; not so good, as I wrote above


;the first loop: if the first element of lst is greater than max, we replace the max with the first element. (process (car lst) (cdr lst)) update the max
;to first element and compare to the rest element. <= which is our invariant. Else the max unchanged, still true for our invariant 
;The k loop: assume (process (car lst) (cdr lst)) update the max number by comparing the first element of sub lst and the rest of lst. For k+1 loop, it is obvious
;we are comparing the first element of lst(before) and (process (car lst) (cdr lst)). Same for else.
; preconditon: lst1, lst2 are the list contains numbers
; postcondition: return true if it is less than (base on greatest number in the list), otherwise it return t









; Problem 2 (20 points)

; Background: Let p? be an order predicate, and suppose seq is a list of elements which is
; ordered with respect to p?.  That is, if i and j are legal indices for seq, then i < j implies that

;     (p? (list-ref seq i) (list-ref seq j))

; is true.  For example, if p? is the usual less-than relation on integers, then the list (1 2 3 5 6)
; is ordered with respect to p?.

; The Problem: Design and certify a recursive procedure insert which inputs an element e,
; a sequence seq ordered by p? as we have just described, and the predicate p?, and which returns
; the list consisting of e and the elements of seq, all in p? order.  That is, the procedure should
; insert the element e into its p?-appropriate position in seq.  For example, if e were 4, then for p? = <
; and the list shown above, the returned list is just (1 2 3 4 5 6):  that is,
;
;                          (insert 4 '(1 2 3 5 6) <) = '(1 2 3 4 5 6)
(define (insert e seq p?)
  (cond ((null? seq) (list e))
        ((p? e (car seq)) (cons e (cons (car seq) (cdr seq))))  ;;;; can you spot a shorter way to write (cons (car seq) (cdr seq))?
        (else (cons (car seq) (insert e (cdr seq) p?)))))
(insert 4 '(1 2 3 5 6) <)
; proof
; prove by inducition
; basic step: when seq is null, the code returns the list contain e. which is correct! (insert into a empty lst)
; Induction Step: assume k times is works(IH) and proof k+1 times is works
;                 therefore, at k times return a list which the e will be on the position of k of lst. on the kth,(insert e (cdr seq) p?)) returns a list
;                 which the first element is e, the rest of elements are >(as example of p?) than e
;                 For k + 1 times, (cons (car seq) (k times loop), since (k times loop) return a lst of e to kth position, when we (cons (car seq), it
;                 becomes k+1 position of lst.


;;;; why should e go to the kth position?  



; Problem 3a (10 points)

; Using the insert procedure developed in 2a, write a sorting procedure insertion-sort which inputs
; a list l of elements and an order predicate appropriate for these elements, and which returns a list
; l' with the same elements as l, but in sorted order.  Thus if l = '(d b a c) and the order predicate
; specifies a < b < c < d, the returned list is '(a b c d).  (A proof is not requested.)
(define (insertion-sort lst p?)
  (define (process lst p? result)
    (cond ((null? lst) result)
          (else (process (cdr lst) p? (insert (car lst) result p?)))))
  (process lst p? '()))
(insertion-sort '(1 2 3 4) >)

; Problem 3b (10 points)

; Finally, using the functions you have developed in the previous problems, write a procedure that inputs
; a list of lists of numbers and which returns the list with the largest number in it.  Calling this
; function list-with-largest-number,  (list-with-largest-number '((16 43 7) (25 98) (57 2 89 14))) should
; return '(25 98).  (A proof is not requested)
(define (list-with-largest-number lst)
  (define (process lst max_lst)
    (cond ((null? lst) max_lst)
          ((list-less-than? max_lst (car lst)) (process (cdr lst) (car lst)))
          (else (process (cdr lst) max_lst))))
  (process lst '(0)))

(list-with-largest-number '((16 43 7) (25 98) (57 2 89 14)))


;;;; works, but you have not made full use of the code you wrote above.  Can you see how to shorten this by calling insertion-sort with the right order predicate?


; Problem 4 (40 points)

; In this problem, you are asked to implement and prove correct a procedure bfs for breadth-first
; search of a binary tree.  For emphasis;  BREADTH FIRST, NOT DEPTH FIRST!

; Binary trees have nodes and branches, and leaves.  Leaves can be represented as atoms; binary trees
; which are not just leaves can be represented as lists of the form

;             (node left-subtree right-subtree)

; For example, (13 (5 6 1) (45 7 18)) is a binary tree with root node 13, and left and right subtrees.
; (5 6 1) is a binary tree with root node 5, left subtree 6 (which is a leaf), and right subtree 1 (also a leaf).

; Given a binary tree t, and an element e, your bfs program is to return #t if e occurs as a node or leaf element of t,
; and #f otherwise.  Thus, referring to the example just given as sample-tree, (bfs sample-tree 0) is #f, while
; (bfs sample-tree 13), (bfs sample-tree 5) and (bfs sample-tree 18) are all #t. 

; You may assume that nodes and leaves are atoms.

; You will want to write selectors (node, leaf?, subtrees) appropriate for this data structure and then to make use of these in your
; breadth-first search program. A constructor, say make-tree, could also be put to good use in setting up test data
; for your function.   


; Recall from your algorithms course that breadth-first search proceeds by maintaining a list of 
; the subtrees that must be returned to:

;   When a leaf is reached, it is examined - if it is the searched for element, return #t; otherwise, search the list of remaining subtrees.

;   When a node is reached, it is examined - if it is the searched for element, return #t; otherwise, add its subtrees to the end of the
;   search list, and then continue the search.

;   If the search list is empty, the search has failed.

; You should define a function aux, with one parameter, search-list, so that bfs can be defined in terms of aux, as follows:


(define (bfs t e)
  (define (root t)
    (car t))
  (define (left t)
    (car (cdr t)))
  (define (right t)
    (car (cdr (cdr (t)))))
  (define (aux search-list)
    (cond ((null? search-list) #f)
          ((equal? (root t) e) #t)
          ((not (equal? (root t) e)) (if (equal? (root (left t))) #t (if (equal? (root(right t))) #t (or (aux (left t)) (aux (right t))))))))  
(aux (list t)))


;;;; the last line of aux is not right -- you need to arrange to put the subtrees at the far end of the search list.

;;;; the or will not work the way I think you want it to, as (aux (left t)) will complete before (aux (right t)) is tried

 
;proof
; root t return root of t, left t return the left subtree of t, right t return the right subtree of t
; the defination of bfs is check the all the chidren of the root first then move on.
; proof by induction:
; basic step: when tree is null, we return false which is obvious true, since we cannot find a element in a empty tree.
; inductive step: I follow the defination of bfs. The sturcture of my design was check root first, check left children and check right children and move to
; the next node. Assume k time works (IH) => k + 1 times works. For k times loop, it will returns true if it find e or false it will not.
; for k + 1 times, it means the tree has one level deeper than k times tree. Therefore, base on our IH, it will the lowest level of tree, will follow the code
; ((not (equal? (root t) e)) (if (equal? (root (left t))) #t (if (equal? (root(right t))) #t #f))), which this check root and check the children for one node.
; the aux left returns the check for the left sub tree, and the aux right returns the right sub tree.
; Here, t is the binary tree, which is searched for the element e -- where e, if it occurs at all in t, occurs as either a node or a leaf. 


;;;; yes, you could organize this as an induction on level, as you say.  Your proof needs some more work, however, to pull this off -- this is where the
;;;; 'synergy' thing comes in:  from problems with the proof, you might have figured out how to fix your code, from which you might have seen how to fix your proof





















