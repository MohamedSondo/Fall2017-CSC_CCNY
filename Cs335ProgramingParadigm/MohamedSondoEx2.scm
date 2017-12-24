
; An L-expression is an S-expression which is either:
;   (AND l1 l2), or
;   (OR l1 l2), or
;   (NOT l1), or

;;;;;;;;;;;;;definition
;list ::= () | (atom ... atom) 
; s-exp ::=  atom | () | (s-exp ... s-exp)

; correctness of lat?, as well as alt-lat?,  can  be established by 
; induction on the length of lst.
;lat ::= () | (cons atom lat)

(define lat?
  (lambda (lst)
    (cond ((null? lst) #t)
          (else 
           (and
            (atom? (car lst)) 
            (lat? (cdr lst)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; any claim of correctness requires a specification:

;   pre:  lst is a list
;
;   post: (lat? lst) = #t when lst is a list of atoms
;                      #f otherwise

; said another way, the post-condition for (lat? lst) is just 'lst is a list of atoms'

; so: one shows by induction on the length of lst that lat? satisfies
; this specification: if the input parameter lst is in fact a list, then 
; (lat? lst) returns #t iff lst is a list of atoms.  Nothing is promised
; if the input parameter is not a list. 

;Selection sorting.
;Remember that selection sort works by finding the smallest element and moving it into the first position, then finding the second smallest and moving it into the second position. In general, on the ith iteration, it finds the ith smallest element and move it to position i. This works as follows:
;(selection '(6 4 5 3 9 3))
;(cons 3 (selection '(6 4 5 9 3))
;(cons 3 (cons 3 (selection '(6 4 5 9))
;(cons 3 (cons 3 (cons 4 (selection '(6 5 9))
;(cons 3 (cons 3 (cons 4 (cons 5 (selection '(6 9))
;(cons 3 (cons 3 (cons 4 (cons 5 (cons 6 (selection '(9))
;(cons 3 (cons 3 (cons 4 (cons 5 (cons 6 (cons 9 (selection '())
;Which becomes (3 3 4 5 6 9).
;The scheme code below does exactly this:
(define (selection L) 
   (cond ( (null? L) '() )
         ( else (cons (smallest L (car L))     ; put the smallest element
                                               ; at the front of the 
                                               ; current list 
                      (selection (remove L (smallest L (car L)))))
                                               ; call selection on the list
                                               ; minus the smallest
                                               ; element
         )
   )
)

(define (remove L A)                ; remove the first occurance of atom A from L
  (cond ( (null? L) '() )           
        ( (= (car L) A) (cdr L))    ; Match found! 
        (else (cons (car L)(remove (cdr L) A)))   ; keep searching
  )
)

(define (smallest L A)             ; looks for the smallest element in the list
                                   ; atom A is the current smallest
  (cond ( (null? L) A)
        ( (< (car L) A) (smallest (cdr L)(car L)))
        (else (smallest (cdr L) A ))
  )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end
;;Insertion sort Start
;; same as the merge function in 
(define (insert L M)
	(if (null? L) M
		(if (null? M) L
			(if (< (car L) (car M))
				(cons (car L) (insert (cdr L) M))
				(cons (car M) (insert (cdr M) L))))))

;; another insert function
;; need to modify the first para in insertionsort function to (car L)
(define (insert2 x L)
	(if (null? L) (list x)
		(let ((y (car L)) (M (cdr L)))
			(if (< x y)
				(cons x L)
				(cons y (insert2 x M))))))

;; Exp. (insertionsort '(4 2 10 3 -1 5)) ==> (-1 2 3 4 5 10)
(define (insertionsort L)
	(if (null? L) '()
		(insert (list (car L)) (insertionsort (cdr L)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end
;;start another insertion sort
;Insertion sort:
(define (insertion lis)
   (cond ((null? lis) '() )
         (else (insert (car lis) (insertion (cdr lis))))
   )
)


(define (insert element lis)
   (cond ((null? lis) (list element) )
         ((> element (car lis))
             (cons (car lis) (insert element (cdr lis))))
         (else (cons element lis))
   )
)

;;proof insertionsort
;proof insertion sort
We will prove the correctness of this sorting algorithm by proving that the loop invariants hold and then drawing conclusions from what this implies upon termination of the loops.
First, we note that Invariant 1 is true initially because in the first iteration of the loop j = 2 so A[1..j-1] is A[1..1] and a single element is always a sorted list.
In order to show that Invariant 1 is maintained by the loop and true during the next iteration, we must examine the body of the loop. It must be true that after the last line of the outer loop (A[i+1] <- key), A[1..j] is a sorted permutation of the original A[1..j]. We will show this is true by examining Invariant 2 and reasoning from it.
Invariant 2 is true upon initialization (the first iteration of the inner loop) because i = j-1, A[i] was explicitly tested and know to be > key, and A[j] == key.

The inner loop maintains this invariant because the statement A[i+1] <- A[i] moves a value in A[i], known to be > key, into A[i+1] which also held a value >= key. Thus this statement does not change the validity of the invariant. (If after setting i <- i-1 the invariant does not hold, the loop test of A[i] > key catches the fact and terminates the loop.)

We also note that the inner loop does not destroy any data because the first iteration copies a value over A[j], the value stored in key. As long as key is stored back into the array, we maintain the statement that A[1..j] contains the first j elements of the original list.

Upon termination of the inner loop, we know the following things about the array A:

    A[1..i] are sorted and <= key (true by default if i == 0, true because A[1..i] is sorted and A[i] <= key if i > 0)
    A[i+1 .. j] are sorted and >= key because the loop invariant held before i was decremented and the invariant said A[i .. j] >= key.
    A[i+1]==A[i+2] if the loop executed at least once and A[i+1] == key if the loop did not execute at all. 

Given these facts, we see that A[i+1] <- key does not destroy any data and gives us A[1..j] is a sorted permutation of the original j elements of A.

Thus, Invariant 1 is maintained after an iteration of the loop and it remains to note that when the outer loop terminates, j = length(A) + 1 so A[1..j-1] is A[1..length], thus the entire array is sorted. 















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end
;;start list-reverse
;; Exp. (list-reverse '(1 (2 3) (4 (5 6) (7 8 (9))))) ==> ((((9) 8 7) (6 5) 4) (3 2) 1)
(define (list-reverse L)
	(if (null? L) L
		(if (list? (car L))
			(append (list-reverse (cdr L)) (cons (list-reverse (car L)) '()))
			(append (list-reverse (cdr L)) (list (car L))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end
;;;start list flatening
;;List flattening
(define (flatten lis)
  (cond ((null? lis) '() )
        ((not (list? (car lis)))  (cons (car lis) (flatten (cdr lis))))
        (else  (append (flatten (car lis)) (flatten (cdr lis))))
   )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end
;;Start of Best MergeSort

;Merge Sort
;Mergesort divides the list in half everytime, calling itself on each half. As the recursion unwinds, it merges the sorted list into a new sorted list. For example, assume we have the following:
;            (mergesort '(6 4 5 7 8 9 3 4))
;                     /                    \
;    (mergesort '(6 4 5 7))                 (mergesort '(8 9 3 4))
;            /             \                   /             \
;(mergesort '(6 4)) (mergesort '(5 7)) (mergesort '(8 9)) (mergesort '(3 4))
;At the lowest tree leves, the two element get sorted and the recursion unwinds.
 ;           (mergesort '(6 4 5 7 8 9 3 4))
;                     /                    \
;    (mergesort '(6 4 5 7))                 (mergesort '(8 9 3 4))
;            /             \                   /             \
;         (4 6)          (5 7)              (8 9))          (3 4))
;The next level of recursion:
;            (mergesort '(6 4 5 7 8 9 3 4))
;                     /                    \
;              (4 5 6 7))                 (3 4 8 9)
;              /     \                     /    \
;           (4 6)    (5 7)              (8 9))  (3 4))
;And the final level with merges into the overall list
;                         (3 4 4 5 6 7 8 9)
;                          /             \
;                      (4 5 6 7)      (3 4 8 9)
;Of course, this is not the actual order that things happen but it give the overall algorithm flavor. In scheme, we can write functions to split lists, and merge sorted lists. Once we have these, the mergesort itself is fairly short.

(define (mergesort L)
  (cond ((null? L)  '())
        ((= 1 (length L)) L)
        ((= 2 (length L)) (mergelists (list (car L))(cdr L)))
        (else (mergelists (mergesort (car (split L)) ) 
                          (mergesort (car (cdr (split L))) ) 
              ))
  )
)

(define (mergelists L M)         ; assume L and M are sorted lists
   (cond ( (null? L) M)
         ( (null? M) L)
         ( (< (car L)(car M))
              (cons (car L) (mergelists (cdr L)M)))
         (else
              (cons (car M) (mergelists L (cdr M))))
   )
)


(define (length L)         ; # elements in a list
   (cond ( (null? L) 0)
         (else (+ (length (cdr L)) 1))
  )
)


(define (sub L start stop ctr)    ; extract elements start to stop into a list
   (cond ( (null? L) L)
         ( (< ctr start) (sub (cdr L) start stop (+ ctr 1)))
         ( (> ctr stop) '() )
         (else
            (cons (car L) (sub (cdr L) start stop (+ ctr 1)))
         )
   )
)

(define (split L)                 ; split the list in half:
                                  ; returns ((first half)(second half))
    (let ((len (length L)))
       (cond ((= len 0) (list L L) )
             ((= len 1) (list L '() ))
             (else (list (sub L 1 (/ len 2) 1)(sub L (+(/ len 2)1) len 1)
                   )
             )
       )
     )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end
;;Start another good mergesort
;; Exp. (merge '(1 3 5 7 8 9 10) '(2 4 6)) ==> (1 2 3 4 5 6 7 8 9 10)
(define (merge L M)
	(if (null? L) M
		(if (null? M) L
			(if (< (car L) (car M))
				(cons (car L) (merge (cdr L) M))
				(cons (car M) (merge (cdr M) L))))))

;; split helper functions
(define (odd L)
	(if (null? L) '()
		(if (null? (cdr L)) (list (car L))
			(cons (car L) (odd (cddr L))))))
(define (even L)
	(if (null? L) '()
		(if (null? (cdr L)) '()
			(cons (cadr L) (even (cddr L))))))

;; Exp. (split '(a b c d e f g h i)) ==> ((a c e g i)(b d f h))
(define (split L)
	(cons (odd L) (cons (even L) `())))

;; Exp. (mergesort '(8 1 3 9 6 5 7 2 4 10)) ==> (1 2 3 4 5 6 7 8 9 10)
(define (mergesort L)
	(if (null? L) L
		(if (null? (cdr L)) L
			(merge
				(mergesort (car (split L)))
				(mergesort (cadr (split L)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end
;;start mergesort from wiki  start here
define (merge-sort l gt?)
  (define (merge left right)
    (cond
     ((null? left)
      right)
     ((null? right)
      left)
     ((gt? (car left) (car right))
      (cons (car right)
            (merge left (cdr right))))
     (else
      (cons (car left)
            (merge (cdr left) right)))))
  (define (take l n)
    (if (zero? n)
      (list)
      (cons (car l)
            (take (cdr l) (- n 1)))))
  (let ((half (quotient (length l) 2)))
    (if (zero? half)
      l
      (merge (merge-sort (take      l half) gt?)
             (merge-sort (list-tail l half) gt?)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;end wiki

;;;Start new  best with proof Mergesort
; Split: input is a list of objects, output is a list of two lists of
;   approximately equal size such that the set of all objects in both
;   lists is the same as the set of objects in the input list.

(define split
  (lambda (l)
    (if (null? l)
        (list '() '())
        (if (null? (cdr l))
            (list l '())
            (let ((rest (split (cdr (cdr l)))))
              (list (cons (car l) (car rest))
                    (cons (car (cdr l)) (car (cdr rest)))))))))

; Merge: input is two lists of numbers in increasing order, output is
;   a list of numbers in increasing order consisting of exactly those
;   numbers in the input list.

(define merge
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (if (null? l2)
            l1
            (if (< (car l1) (car l2))
                (cons (car l1) (merge (cdr l1) l2))
                (cons (car l2) (merge (cdr l2) l1)))))))
                
; Merge-sort: input is a list of numbers, output is that list sorted

(define merge-sort
  (lambda (l)
    (if (null? l)
        '()
        (if (null? (cdr l))
            l
            (let ((s (split l)))
              (let ((a (car s)) (b (car (cdr s))))
                (merge (merge-sort a) (merge-sort b))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;end 

;;; -----------------------------One Merge Sort in Scheme
;;; Merge two lists of numbers which are already in increasing order

  (define merge-lists
    (lambda (l1 l2)
      (if (null? l1)
          l2
          (if (null? l2)
              l1
              (if (< (car l1) (car l2))
                  (cons (car l1) (merge-lists (cdr l1) l2))
                  (cons (car l2) (merge-lists (cdr l2) l1)))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in even positions

  (define even-numbers
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              '()
              (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in odd positions

  (define odd-numbers
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              (list (car l))
              (cons (car l) (odd-numbers (cdr (cdr l))))))))

;;; ---------------------------------------------------------------------
;;; Use the procedures above to create a simple and efficient merge-sort

  (define merge-sort
    (lambda (l)
      (if (null? l)
          l
          (if (null? (cdr l))
              l
              (merge-lists
                (merge-sort (odd-numbers l))
                (merge-sort (even-numbers l)))))))

;;; -------------------------------------------------------
;;; examples (the semi-colons are only for commenting)

  (define a (even-numbers '(2 7 6 5 4 5 6 7 4)))
  ;(7 5 5 7)
  (define b (odd-numbers '(2 7 6 5 4 5 6 7 4)))
  ;(2 6 4 6 4)
  (define c (merge-sort '(3 4 5 2 3 8 9 70 34 23 12 3 45 34)))
  ;(2 3 3 3 4 5 8 9 12 23 34 34 45 70)

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

;;;; abu merge sort proof


;proof merge sort
Now that we know Merge works correctly, we will show that the e
ntire algorithm works correctly,
using a proof by induction
.For the base case, consider an array of 1 element (which is the base case of the algorithm). Such an array is already sorted, so the base case is correct.
For the induction step, suppose that MergeSort will correctly sort any array of length less than n
.  Suppose we call MergeSort on an array of size n
.  It will recursively call MergeSort on two arrays of size n/2
. By the induction hypothesis, these calls will sort these arrays correctly. Hence,after the recursive calls, array a will be sorted between indices
p, . . . m and m+1, . . . qrespectively. We have already showed that merge works correctly, hence after executing it, a
will be correctly sorted between p and q .This concludes our proof. Recall that when you design recursive algorithms, you have to “put your faith” in the recursion,
assume it will work, then specify the processing that follows it. Induction basically gives you the mathematical tool to prove that your “faith leap” is indeed justified.





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end
;;;;;;;;;;;;;;;;;;;;Start BTS
;Binary Tree sort
;The binary tree sort works by building a binary search tree from the input and then traversing it. A binary search tree has the following property at each tree node with value V:
;any nodes in the left subtree have values < V
;any nodes in the right subtree have values >= V
;If we can build such a tree from the input, than a inorder visit finds the elements in the correct order.

; We are going to represent each node in a binary tree as:
;  (value (left child)(right child))  If the node has no left 
;  and/or right child, this is represented with ().  So,
;  (2 (1()())(3()()))  is the tree:   2
;                                   /   \
;                                  1     3


(define (binarysort L)
  (if (null? L) '()
     (traverse (btree L))
))

(define (btree L)
  (if (= (length L) 1) (leaf (car L))
        (binsert (btree (cdr L)) (car L))
  )
)


(define (binsert T A)     ; insert A into the tree
   (cond  ( (null? T) (leaf A) )        ; insert here
          ( (> (car T) A) (list (car T) 
                                (binsert (car (cdr T)) A)
                                (car (cdr (cdr T))))
           )  ; left subtree 
          ( else (list (car T)
                       (car (cdr T)) 
                       (binsert (car (cdr (cdr T))) A))     ; right subtree
          )
    )
)

(define (leaf A)          ; add a leaf to the tree (A ()())
   (list A '() '())
)

(define (traverse L)   ; output sorted list by traversing the tree 
  (cond ( (null? L) L)
        ( else
           (append (traverse (car (cdr L)))
                   (cons (car L)(traverse (car (cdr (cdr L))))))
        )
  )
)

(define (length L)
  (if (null? L) 0
     (+ 1 (length (cdr L)))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end
;;  start More scheme practice
(define (lookup l n)
  (if (> n 0) (lookup1 l n 1)
        -1)
)

(define (lookup1 l n ctr)
    (cond ( (null? l) -1 )
          ( (= ctr n) (car l) )
          ( else (lookup1 (cdr l) n (+ ctr 1)) )
    )
)


(define (update l n x)
   (update1 l n x 1)
)

(define (update1 l n x ctr)
  (cond ( (null? l) '() )
        ( (= ctr n) (cons x (cdr l)) )
        ( else (cons (car l) (update1 (cdr l) n x (+ ctr 1)))   )
   )
)
;;;;; another Selection Sort
(define (remove_item l item)
   (cond ( (null? l) l)
         ( (= (car l) item) (cdr l) )
         (else (cons (car l) (remove_item (cdr l) item))  )
   )
)


(define (smallest l item)
    (cond ( (null? l) item)
          ( (<= item (car l))   (smallest (cdr l) item) )
          (else (smallest (cdr l) (car l)))
    ) 
)

(define (selection l)
   (cond ( (null? l) l )
         ( else
             (let  ((s (smallest (cdr l)(car l))))
                  (cons s (selection (remove_item l s)))
             )
         )
    )
)

;;;;;;;;;;;;;;;;;;;;;;end
;; Start btree
(define (printtree tree)
   (if (null? tree) '()
       (append (printtree (cadr tree)) (list (car tree))(printtree (caddr tree)))
   ) 
)

(define (addtree tree new)
    (cond ( (null? tree) (list new '() '()) )
          ( (<= new (car tree) )  
                (list (car tree) (addtree (cadr tree) new)(caddr tree)) )
          (else
                (list(car tree) (cadr tree) (addtree (caddr tree) new)) )
    )
)


(define (btreesort lis)
   (printtree (treesort lis))
)

(define (treesort lis)
  (if (null? lis) '() 
      (addtree (treesort (cdr lis)) (car lis))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end
;;Start quicksort
Partition function for quicksort
(define (partition lis)    ; ((<= part) part (> part)) will be returned
            (part (cdr lis) (car lis))
)



(define (part lis pivot)
  (cond
    ((null? lis) '(() pivot ()) )
    (else 
      (let   ( (r (part (cdr lis) pivot ) ) )   ; recursive call which will
                                                ; return a list with structure
                                                ; ( (<= pivot) pivot (> pivot))
             (cond
               ((> (car lis) pivot)  ; add to > list
                 (cons (car r) (list pivot (cons (car lis) (car (cdr (cdr r)))))
)
               )
               (else     ; add to <= list
                  (cons (cons (car lis) (car r)) (cdr r))
               ) ; else
             ) ;cond 
      ) ; let
    ) ; else
  )  ; cond
)

(define (quicksort lis)
   (cond ((null? lis) '() )
         ((null? (cdr lis)) lis)
         (else
           (let  ((new (partition lis)) )
             (append (quicksort (car new))
               (cons (car (cdr new))             ; pivot element
                  (quicksort (car (cdr (cdr new))))
               )
             )
           )
         )  ;else
   ) ;cond
) ; fun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;end quicksort
; proof quick sort
Base case: Consider an array of just one element. Quicksort will not do anything, as it should
(the array is sorted)
Induction step: We assume that the recursive calls work correctly (put your faith in the recursion!).
So after these, the two halves of the array are sorted. Assume also that the partition method
works correctly (we will show this below). Now consider two elements of a with indices i < j.
Are they in the correct order?
• If i < j ≤ q then a[i] ≤ a[j] by the induction hypothesis
• If q < i < j then a[i] ≤ a[j] by the induction hypothesis
• If i ≤ q < j or i < q ≤ j then, by using the fact that partition works correctly, we know that
a[i] ≤ x ≤ a[j].
So we checked that any two elements must be in the correct order.
Now let’s prove that partition works correctly. First, we need to check that i and j do not go
out of bounds. To do this, note that we start outside and the first move is correct. Second, there
will be at least one element ≤ x that is put in the second half of the array (this is the first element).
So i, which always increases, will have to hit this element at some point and it will stop, before it
goes out of bounds (even without the first condition). For j we can make a similar argument; either
an element ≤ x exists (or was swapped) in the first half, in which case j will stop on it, or if not, j
will stop on the first element of the array (and will not go out of bounds).
Second, we will show that the algorithm has a loop invariant: whenever we go through the
while loop, it is true that all elements with indices ≤ i are ≤ x and all the ones with indices > j are
≥ x. Since this is true all the time, it will also be true when the algorithm terminates, which will
conclude our proof. This property is clearly true based on the two loops in which i and j move.
When i and j stop, the property is violated for those respective elements, so it is fixed with a swap.
;;;;;;;;;;;;;;;;;end quick sort proof.














;;;;;start BFS
(define (breadth-first-search item tree) 
  (let ((current-tree (first tree))) 
    (display tree) 
    (newline) 
    (cond ((null? tree) #f)                      ;; search ended 
          ((atom? current-tree)                  ;; leaf 
           (or (equal? item current-tree) 
               (breadth-first-search item (cdr tree)))) 
          ((equal? item (first current-tree)))   ;; item found 
          (else 
           (breadth-first-search 
            item 
            (append 
             (rest tree) 
             (list 
              (second current-tree) 
              (third current-tree)))))))) 
(breadth-first-search 'f '((13 (a b c) (d e f)))) 
and linear: 
(define (traverse-tree item tree) 
  (display tree) 
  (newline) 
  (cond ((null? tree) #f) 
        ((atom? tree) (equal? item tree)) 
        ((equal? item (first tree)) #t) 
        (else 
         (or 
          (traverse-tree item (second tree)) 
          (traverse-tree item (third tree)))))) 
(display (traverse-tree 'f '(13 (a b c) (d e f))))

;f each node has a key associated with it, then, the breadh-first 
;search would be faster.  It would beO(LOGmN) 
;where m is the  average number of branches per node, and N 
;is the number of items in the tree.  In such a tree, of course, depth first 
;search would not make any sense. 
;;;;;;;;;;;;end BFS

;;;;another bfs
;4 bfs

(define (bfs t e)
  (define (root t)
    (car t))
  (define (left t)
    (car (cdr t)))
  (define (right t)
    (car (cdr (cdr t))))
  (define (aux search-list)
    (cond ((null? search-list) #f)
          ((equal? (root t) e) #t)
          ((not (equal? (root t) e)) (if (equal? (root (left t)) e) #t (if (equal? (root(right t)) e) #t (or (aux (left t)) (aux (right t))))))))  
(aux (list t)))

(bfs '(13 (5 6 1) (45 7 18)) 10)
;;;;;end another bfs

; kinf of binary search

(define (binary-search lst elem)
  (define mid (floor (/ (length lst) 2)))
  (define midelem (list-ref lst mid))
  
  (if (null? lst)
     #f
     (cond
       ((and (= (length lst) 1) (not (equal? (first lst) elem))) #f)
       ((= midelem elem) #t)
       ((> midelem elem) (binary-search (take lst mid) elem))
       ((< midelem elem) (binary-search (drop lst mid) elem))
       (else #f)
     )
   )
)
;;;end kind of binary search


;;;;;;;;;;;;;;;;star Dfs

 ;    We have a directed graph G = (A,V) and we assume that all
;     vertices have an attribute called (mark v), where (mark v) is set to
  ;   "unvisited" for all vertices v in (V G). 

   ;  Select some vertex of (V G), call it s, as a starting vertex and mark 
   ;  vertex s as visited.

    ; Each vertex w adjacent to s that has (mark w) equal to "unvisited" is
    ; then searched in turn using depth first search recursively.

    ; Once all vertices that can be reached from s have been visited the
     ;search from s is complete

(define (recursive-dfs s G)
  (let ((mark (make-vector (+ 1 (n G)) #f))                          ;(1)
	(visited '()))                                               ;(2)
  (define (dfs' w)                                                   ;(3)
    (cond ((not (vector-ref mark w))                                 ;(4)
	   (vector-set! mark w #t)                                   ;(5)
	   (set! visited (cons w visited))                           ;(6)
	   (do ((unvisited (adjacent-to w G) (cdr unvisited)))       ;(7)
	       ((null? unvisited))                                   ;(8)
	     (dfs' (first unvisited))))))                            ;(9)
  (dfs' s)                                                           ;(10)
  visited))                                                          ;(11)
;;;
;;; Depth first search from starting vertex s in graph G
;;;
;;; (1) Vector element (vector-ref mark w) is #t if vertex w has been visited
;;;     otherwise (vector-ref mark w) is #f
;;; (2) visited is a list of vertices, giving the vertices in reverse 
;;;     order as visited by a dfs from vertex s in G
;;; (3) Local recursive function dfs' ... this is for cosmetics
;;;     saving us having to pass in the vector mark every call
;;; (4) If we have not already visited the vertex w ...
;;; (5) ... then set w's mark ast true ...
;;; (6) ... push w onto the set of visited vertices ...
;;; (7-9) ... then make recursive calls to dfs' from the vertices 
;;;           adjacent to w in G
;;;
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Prep for test2
(define (insertR* new old l)
  (cond ((null? l) l)
        ((pair? (car l)) (cons (insertR* new old (car l))
                               (insertR* new old (cdr l))))
        ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
        (else (cons (car l) (insertR* new old (cdr l))))))

(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

(define (replace-nth lst n old new)

  ; counts occurrences of atom inside the tree defined by lst
  (define (count-occur lst atom)
    (cond  ((null? lst) 0)
           ((not (pair? (car lst))) (+ 1 (count-occur (cdr lst) atom)))
           (else (+ (count-occur (car lst) atom)
                    (count-occur (cdr lst) atom)))))

  (cond ((null? lst) '())
        ((pair? (car lst)) (cons (replace-nth (car lst) n old new) 
                                 (replace-nth (cdr lst) (- n (count-occur (car lst) old)) old new)))
        ; so car lst is an atom
        ((eq? (car lst) old) (if (= n 1) (cons new (cdr lst))
                                         (cons (car lst) (replace-nth (cdr lst) (- n 1) old new))))
        (else (cons (car lst) (replace-nth (cdr lst) n old new)))))

(replace-nth '(1 1 1 1 2) 1 1 4)
(replace-nth '(1 (1 1 1) 1 2) 2 1 4)
(replace-nth '(1 (1 1 1) 1 2) 3 1 4)
(replace-nth '(1 (1 1 1) 1 2) 4 1 4)
(replace-nth '(1 (1 1 1) 1 2) 5 1 4)
(replace-nth '(1 (1 1 1) 1 1 2) 5 1 4)
(replace-nth '(1 (1 1 1) 1 1 2) 6 1 4)
(replace-nth '(1 (1 (1 1)) (1) 1 2) 3 1 4)

(define (replace-nth lst n old new)

  ; counts occurrences of atom inside the tree defined by lst
  (define (count-occur lst atom)
    (cond  ((null? lst) 0)
           ((not (pair? (car lst))) (+ 1 (count-occur (cdr lst) atom)))
           (else (+ (count-occur (car lst) atom)
                    (count-occur (cdr lst) atom)))))

  (define (derp lst n oldn old new)
    (cond ((null? lst) '())
          ((pair? (car lst)) (cons (replace-nth (car lst) n old new) 
                                   (replace-nth (cdr lst) (- n (count-occur (car lst) old)) old new)))
          ; so car lst is an atom
          ((eq? (car lst) old) (if (= n 1) (cons new (replace-nth (cdr lst) oldn old new))
                                 (cons (car lst) (replace-nth (cdr lst) (- n 1) old new))))
          (else (cons (car lst) (replace-nth (cdr lst) n old new)))))

  (derp lst n n old new))

(replace-nth '(1 1 1 (1 1 2) 1 3) 2 1 4)

(define (atom? x)
  (not (or (null? x) (pair? x))))

(define (deep-reverse x)
  (cond ((null? x) '())
        ((atom? (car x)) (cons (deep-reverse (cdr x)) (car x)))
        (else (cons (cons (deep-reverse (cdr x)) (deep-reverse (car x))) '()))))

(deep-reverse '(1 2 (1 2) (1 (1 2))))

(define (flatten tree)
  (cond ((null? tree) '())
        ((pair? tree) (append (flatten (car tree)) (flatten (cdr tree))))
        (else (list tree))))

(flatten '(1 2 (1 2 (1 2))))
(append '(1 2 3) '(4 5 6))

(define (my-reverse lst)
  (cond ((null? lst) '())
        (else (cons (car lst) (my-reverse (cdr lst))))))

(define (my-reverse lst)
  (define (iter lst 

(my-reverse '(1 2 3 4))
;;;;;;;;;;;;;;;;;end Prep for test 2


;;;;Deep Member start here.
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
(deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))
;;;;;;;;;;;;end Deep Menber
;;menber
(define member?
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
;;;;;end menber
; use cons to remove member

(define rember2
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond 
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat) (rember2 a (cdr lat)))))))))

; carry out some testing

;(rember2 'and '(bacon lettuce and tomato))

; ...

; when everything seems to be right, work up a proof.  the idea is
; to exploit whatever techniques are available to increase one's 
; confidence in the code


; we develop an induction argument showing that rember2
; satisfies its specification

; as before, the induction is on the length of lat

; again, the basis step -- for lat of length 0 -- is clearly correct: 
;  (rember2 a ()) = () , exactly as it ought

; assuming that the recursive call (rember2 a (cdr lat)) works correctly,
; we now need to argue that the else clause computes the desired result.

; one begins by observing that there are two cases: either the atom a 
; occurs first in lat, or not.  If it does occur first, then the post
; condition is achieved if our function returns (cdr lat), as you can 
; see it does.  If the atom a does not occur first, then either it occurs
; later in lat or it does not occur at all.  In either case, the induction
; hypothesis (namely, that (rember2 a (cdr lat)) works correctly) guarantees
; that (rember2 a lat) works correctly.  

; how does this argument show that it is the _first_ occurrence
; which has been removed?  it may make sense to fill in some of the 
; detail in induction step:

;   If the atom a does not occur first, then either it occurs 
;   later in lat or it does not occur at all.  In the first case,
;   the first occurrence of a in (cdr lat) is the first occurrence
;   of a in lat, so it is enough to know that (rember2 a (cdr lat))
;   removes the first occurrence of a in (cdr lat), as is guaranteed
;   by the induction hypothesis.  If a does not occur at all, then
;   of course it does not occur in (cdr lat), and the induction 
;   hypothesis guarantees that ...
;; end rmenber.


;;;; remove left-most
(define (remove-left-most predicate tree)

  (cond ((null? tree) '())
        ((predicate (car tree)) (cdr tree))
        ((deep-member? predicate (car tree)) (cons (remove-left-most predicate (car tree))
                                                   (cdr tree)))
        (else (cons (car tree) (remove-left-most predicate (cdr tree))))))

(remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e)))))))
;;Proof of right left most.


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

(odd-indexed-elements '(a b))
(odd-indexed-elements '(a b c d e f g h))
(even-indexed-elements '(a b c d e f g h))
1b.  (10 points) Prove that both functions are correct (Hint: use one proof for both function).

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

;;;;;;;;;;;;;;;;start simpson
(define (simpson f a b n)
  (simp-aux f a b n (/ (- b a) (* n 1.0) )))

(define (simp-aux f a b n h)
  (define (term k)
      (cond ((= 0 k) (f a))
        ((= n k) (f (+ a (* k h))))
        ((odd? k) (* 4 (f (+ a (* k h)))))
        ((even? k) (* 2 (f (+ a (* k h)))))
        ))
  
  (define (next k)
    (+ k 1))

  (* (/ h 3.0)
     (sum term 0 next n))

  )
    
    
(simpson cube 0 1 100)

(simpson cube 0 1 1000)

(simpson cube 0 1 10000)
;;;;;;;;;;;;;;;;end simpsons.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Start list length and more
; length resurcive


(define (mylength l)
  (cond ((null? l) 0)
	(else (+ 1 (mylength (cdr l))))))

; iterative:
(define (my-length-iter l)
  (define (iter l result)
    (cond ((null? l) result)
          (else (iter (cdr l) (+ result 1)))))

  (iter l 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; list-ref

(define (mylistref n l)
  (define (aux count l)
    (cond ((= count n) (car l))
	  (else (aux (+ count 1) (cdr l)))))
  (aux 0 l))


; can we implement list-ref without having a separate count parameter?

; pre: l is a non-empty list
; pre: n is a legitimate index for l
;recursive
(define (mylistref n l)
  (cond ((= n 0) (car l))
	(else (mylistref (- n 1) (cdr l)))))


; iterative:
(define (my-list-ref-iter l index)
  (cond ((>= index (length l)) '())
        ((= index 0) (car l))
        (else (my-list-ref-iter (cdr l) (- index 1)))))

; invariant-based proofs developed and discussed, in detail

; for the first, we consider
;   the (element in the) Nth position in L is (element in the) the count-th position in l
; and noted that this cannot be correct.  But
;   the Nth position in L is the (N-count)th position in l
; is more plausible.

; work out the details!


; for the second, we consider
;  the Nth position in L is the nth position in l

; again, it is a good idea to work out the complete argument


; last, all-but-last

; assumes l is a non-empty list
(define (last l)
  (cond ((null? (cdr l)) (car l))
	(else (last (cdr l)))))


; assumes l is a non-empty list
(define (all-but-last l)
  (cond ((null? (cdr l)) '())
	(else (cons (car l) (all-but-last (cdr l))))))




; map

(define (mymap f l)
  (cond ((null? l) l)
	(else (cons (f (car l)) (mymap f (cdr l))))))

(define (square x) (* x x))

; examples ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; accumulate

(define (accumulate op init seq)
  (cond ((null? seq) init)
	(else (op (car seq) (accumulate op init (cdr seq))))))


; examples ...

; filter

(define (filter pred seq)
  (cond ((null? seq) seq)
	((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
	(else (filter pred (cdr seq)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; depth

(define (depth tree)
  (cond ((null? tree) 0)
	((atom? tree) 0)
	(else (max (+ 1 (depth (car tree)))
		   (depth (cdr tree))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; count leaves

(define (count-leaves tree)
  (cond ((null? tree) 0)
	((atom? tree) 1)
	(else (+ (count-leaves (car tree))
		 (count-leaves (cdr tree))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; fringe

(define (fringe tree)
  (cond ((null? tree) '())
	((atom? tree) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))

;; count tree

(define (count-nodes tree)
  (+ 1 (accumulate + 0 (map (lambda (subtree)
			      (if (pair? subtree)
				  (count-nodes subtree)
				  1))
			    tree))))

;;;;;;;;;;;;;;;;;;insertR

(define (insertR lat occ new)
  ... )
;For instance, one might show 
;   (insertR '(a b c d c e f) 'c 'new) = '(a b c new d c e f)
; and
;   (insertR '(a b c d c e f) 'g 'new) = '(a b c d c e f)

; where the specification is 
;
;  pre:  lat is a list of atoms
;        occ is an atom, which may or may not occur in lat
;        new is an atom
;
;  post: given that lat = (a1 a2 ... am) and that aj is the first occurrence of occ in lat, 
;             (insertR lat occ new) =  (a1 a2 ... a(j-1) occ new a(j+1) ... am)
;
;        if occ does not occur in lat, then 
;             (insertR lat occ new) = lat


; one possible design

(define (insertR lat occ new)
  (cond ((null? lat) '())
	((eq? (car lat) occ) (cons (car lat) 
				   (cons new (cdr lat))))
	(else (cons (car lat) (insertR (cdr lat) occ new)))))



(define (multisubst lat old new)
  (cond ((null? lat) '())
	((eq? (car lat) old) (cons new (multisubst (cdr lat) old new))) 
	(else (cons (car lat) (multisubst (cdr lat) old new)))))


; do you see the similarity of insertR, insertL, subst, and multisubst?

; could you convince yourself that you do --  by now writing out for yourself 
; designs of multi-insertR and multi-insertL?  

; make sure you see how to give specifications for the multi- functions!


;;;;same Shape

(define same-shape? 
  (lambda (l1 l2)
    (cond 
      ((or (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
      (else
       ; both l1 and l2 are pairs
       (and (same-shape? (car l1) (car l2))
            (same-shape? (cdr l1) (cdr l2)))))))

;;flatmap section

; given a positive integer n, find all ordered pairs of distinct positive integers i and j,
; where 1 <= i < j <= n

(define (ordered-pairs-of-distinct-integers n)
  (accumulate append
	      '()
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval (+ i 1) n)))
		   (enumerate-interval 1 (- n 1)))))


; what is the effect of accumulate-append?  Consider, for contrast,

(define (another-ordered-pairs n)
  (map (lambda (i)
	 (map (lambda (j) (list i j))
	      (enumerate-interval (+ i 1) n)))
       (enumerate-interval 1 (- n 1))))


; flatmap allows a simplification

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (ordered-pairs-of-distinct-integers n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval (+ i 1) n)))
	   (enumerate-interval 1 (- n 1))))



; flatmap turns out to be quite useful - next we use it to compute
; all permutations of a set S

; for example, the permutations of {1,2,3} are given --
; first, list all permutations with 1 in the first position
; next, list all permutations with 2 in the first position
; finally, list all permutations with 3 in the first position


(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap
       (lambda (x)
         (map (lambda (p) (cons x p))
              (permutations (remove x s))))
       s)))


; where

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))



; if we define flatten


(define (flatten x)
  (accumulate append '() x))


; then we can write

(define (flatmap proc seq)
  (flatten (map proc seq)))

;; end flatmap section


;;hw5
; this function relies entirely on the assumption that the built-in function length returns the correct length of the provided list. Given this assumption, there is little to prove. Our representation of an unary number n is simply a list of n 1s, which necessarily has a length equal to n.
(define (get-unary num)
  (length num))

; if the number is 0, this function correctly returns an empty list, which can be loosely described as a list consisting of 0 1s.
; otherwise, it returns it returns a 1 appended to a list of n-1 1s, which is the correct return value since it will create a list of n 1s.
(define (create-unary num)
  (cond ((= num 0) '())
        (else (cons 1 (create-unary (- num 1))))))

(define add1
  (lambda (m) (create-unary (+ (get-unary m) 1))))

(define sub1
  (lambda (m) (create-unary (- (get-unary m) 1))))

; 3.  Exercises 2.2, 2.3 and 2.4 in Abelson and Sussman.  Proofs are not necessary.

; 2.2
(define (make-segment p1 p2)
  (list p1 p2))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (car (cdr line)))

(define (make-point x y)
  (list x y))

(define (x-point pt)
  (car pt))

(define (y-point pt)
  (car (cdr pt)))

(define (midpoint-segment line)
  (make-point (/ (+ (x-point (start-segment line)) 
                    (x-point (end-segment line)))
                 2)

              (/ (+ (y-point (start-segment line))
                    (y-point (end-segment line)))
                 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define p1 (make-point 0 0))
(define p2 (make-point 10 10))
(print-point p1)
(print-point p2)
(define l1 (make-segment p1 p2))
(print-point (midpoint-segment l1))

;2.4

(define (cons x y)
  (lambda (m) (m x y))) ; applies function m to arguments x y

(define (car z)
  (z (lambda (p q) p))) ; applies function z to first argument of 

(car (cons x y))
((cons x y) (lambda (p q) p))
((lambda (m) x y) (lambda (p q) p))
((lambda (p q) p) x y)
x

(define (cdr z)
  (z (lambda (p q) q)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;end hw5

;;;hw4
; 3. Write a function start that takes two arguments, lst and num, and which returns the
; first num elements of lst.
; let's assume order doesn't matter...
; recursive:
(define (start lst num)
  (cond ((= num 0) '())
        (else (cons (car lst) (first-num-of-lst (cdr lst) (- num 1))))))

; iterative:
(define (start-iter lst num)
  (define (iter lst num result)
    (cond ((= num 0) result)
          (else (iter (cdr lst) (- num 1) (cons (car lst) result)))))

  (iter lst num '()))

; 4.  Write a function but-last that takes two arguments, lst and num, and which returns the
; list of all but the last num elements of lst.

; recursive:
(define (but-last lst num)
  (cond))

; iterative:
; add elements to result until length of result = length of lst - num
; it's horrifying.
; shh it works (TM)
(define (but-last-iter lst num)
  (define (iter lst num result curr)
    (cond ((= (length result) (- (length lst) num)) result)
          (else (iter lst num (cons (list-ref lst curr) result) (+ curr 1)))))

  (iter lst num '() 0))

(define (but-last l num)

  (define (aux l count derp)
    (cond ((= count max) '())
          (else (cons (car l)
                      (aux (cdr l)
                           (+ count 1)
                           derp)))))
  (aux l 0 (- (length l) num)))

(define (but-last l num)
  (define (aux l count result)
    (cond ((= count  (length (cdr l))) result)
          (else (aux (cdr l) count (cons (car l) result)))))

  (aux l num '()))

; inducting on .... length of l: we show by induction on the length of l that whenever 0 <= num <= (length l), the call (aux l 0 (- length l) num) returns the list consisting of the first
; (- (length l) num) elements of l, in the same order they occur in l.
; induction hypothesis: for all lists l' shorter than l with 0 <= num <= (length l'), (aux l' 0 max) returns all but the last... this proof is poorly-constructed

; remember the design role of your variables. max is the length of... the list we're working with?
; is it better to reformulate the program to simplify the proof?

; 5.  Write a function end that takes two arguments, lst and num, and returns the last num
; elements of lst.
; recursive:

(define (end lst num)
  (cond ((or (= num 0) (null? lst)) '())
        ((= num 1) (cons (car lst) '()))
        ((= num 2) (cons (car (cdr lst) '(cdr 
        (else (cons (end (cdr lst) (- num 1)))))) ; almost definitely wrong but whatever

; iterative 
(define (end-iter lst num)
  (define (iter lst len value count result)
    (cond ((= count len) result)
          ((< count value) (iter (cdr lst) len value (+ count 1) result))
          (else (iter (cdr lst) len value (+ count 1) (cons (car lst) result)))))

  (iter lst (length lst) (- (length lst) num) 0 '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end hw4


;;start interpreter
; we consider the class i-aexp of fully parenthesized
; infix arithmetic expressions, defined inductively as the least class 
; containing the non-negative integers and closed under
; +, * and ^:

; if e1 and e2 are i-aexps, then so are (e1 + e2), (e1 * e2)
; and (e1 ^ e2)

; some examples of i-aexps : 1, 18, (1 + 18), ((1 + 18) * 18) 
; ( ((1 + 18) * 18) ^ (26 * 2) )


; we can of course also give a grammar for i-aexp:

; i-aexp ::= non-negative-integer | (i-aexp + i-aexp)  |
;          (i-aexp * i-aexp) | (i-aexp ^ i-aexp)


; Constructors for fully parenthesized infix expressions 
; are easily given.  The assumption is that e1 and e2 are themselves fully
; parenthesized infix arithmetic expressions (fpiae), so these constructors
; may be applied ('inductively') to such expressions to build more
; complicated expressions

(define make-+
  (lambda (e1 e2)
    (list e1 '+ e2)))

(define make-*
  (lambda (e1 e2)
    (list e1 '* e2)))

(define make-^
  (lambda (e1 e2)
    (list e1 '^ e2)))


; check that the returned value in each case is a fpiae!


; we create some i-aexps using these constructors

(define test-exp-1
  (make-+
   (make-* 4 5)
   (make-^ 2 3)))

; so now test-exp-1 is ((4 * 5) + (2 ^ 3))


(define test-exp-2
  (make-*
   (make-^ 2 3)
   (make-+ 4 5)))


; so test-exp-2 is ((2 ^ 3) * (4 + 5))



; selectors for fully parenthesized infix arithmetic expressions 
; can be defined as follows:

(define operator
  (lambda (i-aexp) 
    (car (cdr i-aexp))))

(define first-operand
  (lambda (i-aexp)
    (car i-aexp)))

(define second-operand
  (lambda (i-aexp)
    (car (cdr (cdr i-aexp)))))



; examples -- in class


; classifiers for i-aexps


(define plus-aexp?
  (lambda (aexp)
    (eq? (operator aexp) '+)))

(define times-aexp?
  (lambda (aexp)
    (eq? (operator aexp) '*)))

(define power-aexp?
  (lambda (aexp)
    (eq? (operator aexp) '^)))



; [this is a common pattern for new data types: define constructors,
; selectors and then classifiers]


; note that we might have other sorts of arithmetic
; expressions -- for example, with operators in prefix
; position (p-aexp), or with some relaxation of the requirement
; for full parenthesization (perhaps nfp-aexp)

; let's consider these as all belonging to the class 
; aexp of arithmetic expressions: each aexp is either
; a number, or it has an operator, and a first operand, 
; and a second operand

; the following procedure requires that we have defined 
; selectors and classifiers appropriate for its argument

; it also requires that procedures plus, times and expon
; have been defined before it can be used.  we give these
; definitions further on

; data abstraction at work!
					;
; inputs an aexp and returns the value of the aexp

(define value
  (lambda (aexp)
    (cond ((natnum? aexp) aexp)
          (else (cond ((plus-aexp? aexp) (plus (value (first-operand aexp))
                                               (value (second-operand aexp))))
                      ((times-aexp? aexp) (times (value (first-operand aexp))
                                                 (value (second-operand aexp))))
                      ((power-aexp? aexp) (expon (value (first-operand aexp))
                                                 (value (second-operand aexp))))
                      )))))


; natnum? might be:

(define natnum?
  (lambda (x)
    (and
     (integer? x)
     (>= x 0))
    ))


; here integer? and >= are pre-defined in scheme


; one possible implementation of plus, times and expon

(define plus 
  (lambda (m n)
    (cond ((= n 0) m)
          (else (add1 (plus m (sub1 n)))))))

(define times 
  (lambda (m n)
    (cond ((= n 0) 0)
          (else (plus m (times m (sub1 n)))))))

(define expon
  (lambda (base exponent)
    (cond ((zero? exponent) 1)
          (else (times base (expon base (sub1 exponent)))))))

; where

(define add1
  (lambda (m) (+ m 1)))

(define sub1
  (lambda (m) (- m 1)))


;;end interpreter

