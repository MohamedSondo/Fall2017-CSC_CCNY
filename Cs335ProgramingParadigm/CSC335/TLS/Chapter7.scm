(define (atom? a)
  (not (or (pair? a) (null? a))))

(define (member? atom ls)
  (cond ((null? ls) #f)
        ((equal? atom (car ls)) #t)
        (else (member? atom (cdr ls)))))

; remove all occurrences of atom from ls
; should probably have one that only does the first occurrence
; you know, for kicks.
(define (rember atom ls)
  (cond ((null? ls) ls)
        ((equal? atom (car ls)) (rember atom (cdr ls)))
        (else (cons (car ls) (rember atom (cdr ls))))))

(rember 'a '(a b c d e))
(rember 'a '(a b a d e))
(newline)

(define (set? l)
  (cond ((null? l) #t)
        ((member? (car l) (cdr l)) #f)
        (else (set? (cdr l)))))

(set? '())
(set? '(apple peaches apple plum))
(set? '(apple peaches pear plum))
(newline)

(define (makeset l)
  (cond ((null? l) l)
        ((member? (car l) (cdr l)) (makeset (cdr l)))
        (else (cons (car l) (makeset (cdr l))))))

(makeset '(apple peach pear peach plum apple lemon peach))
(newline)

(define (makeset-rember l)
  (cond ((null? l) l)
        ((member? (car l) (cdr l)) (cons (car l)
                                         (makeset-rember (rember (car l) (cdr l)))))
        (else (cons (car l) (makeset-rember (cdr l))))))

(makeset-rember '(apple peach pear peach plum apple lemon peach))
(newline)

; #t if l1 is a subset of l2 (l2 contains every element in l1)
(define (subset? l1 l2)
  (cond ((null? l1) #t)
        ; empty set is a subset of every set, but no set is a subset of empty set
        ((null? l2) #f)
        ; cdr down l1, checking if each element in it is in l2
        ((member? (car l1) l2) (subset? (cdr l1) l2))
        ; if we've come this far, neither list is null and l1 contains an element not in l2
        (else #f)))

(subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings)) ; #t
(subset? '(4 pounds of horseradish) '(4 pounds chicken and 5 ounces horseradish)) ; #f
(newline)

(define (subset-and? l1 l2)
  (cond ((null? l1) #t)
        (else (and (member? (car l1) l2)
                   (subset-and? (cdr l1) l2)))))

(define (eqset? s1 s2)
  (cond ((and (null? s1) (null? s2)) #t)
        ; if this is true, one of them is null but not the other. So #f
        ((or (null? s1) (null? s2)) #f)
        ; check if each member of s1 is in s2. Basically the subset function with
        ; the requirement that the two sets be of same size.
        (else (and (member? (car s1) s2)
                   (eqset? (cdr s1) s2)))))

; or note that if two sets are subsets of each other then they must be equal.
(define (eqset? s1 s2)
  (and (subset? s1 s2) (subset? s2 s1)))

; #t if s1 contains at least one element in s2
(define (intersect? s1 s2)
  (cond ((null? s1) #f)
        (else (or (member? (car s1) s2) (intersect? (cdr s1) s2)))))

(intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))
(newline)

; intersection of sets
(define (intersect s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ; if car s1 is also member of s2, cons it onto result of intersect
        ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
        (else (intersect (cdr s1) s2))))

(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
(newline)

(define (union s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ; if car s1 is member of s2, skip it and move on
        ((member? (car s1) s2) (union (cdr s1) s2))
        ; otherwise tack it onto the list
        (else (cons (car s1) (union (cdr s1) s2)))))

(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
(newline)

(define (intersectall l-set)
  ; if there's only one set, intersection of list of sets is just that one set
  (cond ((null? (cdr l-set)) (car l-set))
        ; otherwise, intersect the first set with the intersection of the remaining sets
        (else (intersect (car l-set) (intersectall (cdr l-set))))))

(intersectall '((a b c) (c a d e) (e f g h a b)))
(newline)

(define (a-pair? l)
  (cond ((atom? l) #f)
        ((null? (cdr l)) #f)
        ((null? (cdr (cdr l))) #t)
        (else #f)))

(a-pair? '(pear pear))
(a-pair? '(3 7))
(a-pair? '((2) (pair)))
(a-pair? '(full (house)))
(a-pair? '(a b c d))
(newline)

(define (first l)
  (car l))
(define (second l)
  (car (cdr l)))
(define (build first second)
  (cons first (cons second '())))

(define (third l)
  (car (cdr (cdr l))))

(define (firsts rel)
  (cond ((null? rel) '())
        (else (cons (first (car rel)) (firsts (cdr rel))))))

(define (seconds rel)
  (cond ((null? rel) '())
        (else (cons (second (car rel)) (seconds (cdr rel))))))

(define (rel? l)
  (cond ((null? l) #t)
        (else (and (set? l) (a-pair? (car l)) (rel? (cdr l))))))

(rel? '(apples peaches pumpkin pie))
(rel? '((apples peaches) (pumpkin pie) (apples peaches)))
(rel? '((apples peaches) (pumpkin pie)))
(rel? '((4 3) (4 2) (7 6) (6 2) (3 4)))
(newline)

(define (fun? rel)
  ; no-brainer
  (set? (firsts rel)))

(define (revrel rel)
  (cond ((null? rel) '())
        (else (cons (build (second (car rel)) (first (car rel)))
                    (revrel (cdr rel))))))

(revrel '((8 a) (pumpkin pie) (got sick)))
(newline)

(define (revpair pair)
  (build (second pair) (first pair)))

(define (revrel-fn rel)
  (cond ((null? rel) '())
        (else (cons (revpair (car rel)) (revrel-fn rel)))))

(define (fullfun? rel)
  (and (fun? rel) (set? (seconds rel))))

; exactly the same as fullfun but easier to read... I guess
(define (one-to-one? rel)
  (fun? (revrel rel)))