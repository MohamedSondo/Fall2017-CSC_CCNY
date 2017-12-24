(cons 1 2)
(define foo (cons 1 2))
(define bar (cons (cons 1 2) 3))
(car bar);;(1 . 2)

;; list is set of pair.
(define fbar (cons 1 '()))
(car fbar);; 1
(cdr fbar);;()

;; list a bunch of pairs.
(define mylist'(1 2 3 10 5))
;;(caddr mylist) give 3 we read from right to left.
(equal? (list 1 2 3) mylist)
;; list reference allow us to get a specific index in the list
;;ex list ref buil in function 
(list-ref mylist 1)
;; my list ref check to to see if n is zero, then we return the car of the list
;;else we recursively reduce our list with new call of the original  with our list and reduce n by 1).
;; n will definely get down to zero which is the base base and we will then return the car.

(define (my-list-ref lst n)
  (if (zero? n)(car lst)
      (my-list-ref (cdr lst )(- n 1))))

;;> (my-list-ref mylist 3) return 10
;; Scheme loop


(define (double x)(* x 2))
;; pass a procedure to a procedure
;Map
;; when emply list is passed and ultimaltemy we will reach the empty for our recursion.
;; we cons  function the first in the list which is the car, and on the right which is the result of callin my-map the same function
;; with the rest of list which is the cdr.

(define (my-map fn lst )
  (if (null? lst) '()
      (cons (fn (car lst)) (my-map fn (cdr lst)))))

(my-map double mylist)

;;
(define qux (list 1 2 3 4))

;; foldr take a function, start and the list
;;(foldr + 0 qux)
(define (my-foldr fn start lst)
  (if (null? lst) start
      (fn (car lst) (my-foldr fn start (cdr lst)))))
(my-foldr + 0 qux)
(my-foldr * 1 qux)

;; the function combine 0 given function with last element in the list


cdr (car '(a (b (c)) d))


(define lst '( 1 2 3 4 ))
;; we first check to see if our lst is null, if yes return 0
;;else we add 1 to the recursive call on the cdr, 
(define mylength(lambda(lst)
                (if(null? lst) 0
                (+ 1 (mylength(cdr lst))))))

(mylength lst )
;; if the sum of all the element is empty, we return 0;
;;else we add the car to the call sum list of the cdr of the list.
(define sum-list(lambda(lst)
  (if(null? lst) 0
     (+ (car lst) (sum-list (cdr lst))))))


(sum-list lst)

(define reduce-list(lambda(lst base f)
  (if(null? lst) base
     (f (car lst) (reduce-list (cdr lst)base f)))))


(reduce-list lst 1 *)



