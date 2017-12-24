; Sixth Homework Set
; CSc 335
; Fall 2016


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; First Problem

; replace-nth

; Here is a tree recursion somewhat more complicated than those we have looked at until now


; develop and certify a scheme program replace-nth which takes as input

;        a list lst, not necessarily a list of atoms
;        a positive integer, n
;        an atom, old
;        an atom, new

; (replace-nth lst n old new) should replace the nth occurrence of old in 
; lst by new (and leave everything else unchanged)

; replace nth occurence of old with new in lst

; if null, we can just return. no need to do anything
; if car lst is a pair, we'll need to check inside for occurrences of old
; cons the function run on it to the rest of the list.
; if we find occurrences inside the nested list, this should change the value of n for when we exit.
; e.g. if we find 3 occurrences of old inside the list, and we're looking for the 4th one, n should be 1 when we run replace-nth on the cdr
; if car lst is an atom and an occurrence of old, check if n == 1 (searching for first occurrence)
; if n == 1, change it and append it to the rest of the list.
; should we run the function on the cdr of the list as well? No. We already found and replaced the nth occurrence. We're done.
; if not, tack it on to the front of the list then run the function with (- n 1) and the cdr of the list
; if car is not a list and not equal just tack it on to the front of replace-nth with cdr of the list
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

; tree induction: first look at the car, then the cdr.
; there's another solution: extract the fringe of the tree (flat list), then it's trivial to find nth occurrence, then (hard part) write a program that takes a tree and a new fringe that returns a tree isomorphic to the original one but with the new fringe.
; Additional Problems

; Abelson and Sussman, Exercise 2.27 
(define (deep-reverse x)
  (cond ((null? x) '())
        ((not (pair? x)) (list (deep-reverse (cdr x)) x))
        (else (list (deep-reverse (cdr x)) (deep-reverse (car x))))))
; Abelson and Sussman, Exercise 2.29
; Abelson and Sussman, Exercise 2.32
(define (subsets s)
  (if (null? s) (list '())
    
; Abelson and Sussman, Exercise 2.37
; Abelson and Sussman, Exercise 2.41
; Abelson and Sussman, Exercise 2.42


; We will discuss these problems over a few classes, starting on Tuesday.  You will get much more from the discussion 
; if you have put in some time solving them before class.  Additionally, the second exam will be all but impossible for 
; students who have not managed to solve these problems.  
