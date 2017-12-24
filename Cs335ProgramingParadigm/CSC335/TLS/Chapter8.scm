(define (atom? a)
  (not (or (pair? a) (null? a))))

(define (rember-f test? a l)
  (cond ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) (rember-f test? a (cdr l))))))

(rember-f = 5 '(6 2 5 3))
(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))
(newline)

(define (eq?-c a)
  (lambda (x) (eq? x a)))

(define eq?-salad (eq?-c 'salad))

(eq?-salad 'salad)
(eq?-salad 'something)
(newline)

(define (rember-f test?)
  (lambda (a l)
    (cond ((null? l) '())
          ((test? a (car l)) (cdr l))
          (else (cons (car l)
                      ((rember-f test?) a (cdr l)))))))

(define rember-eq (rember-f eq?))

(define (insertL-f test?)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (cons new l))
          (else (cons (car l)
                      ((insertL-f test?) new old (cdr l)))))))

(define (insertR-f test?)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (cons old (cons new (cdr l))))
          (else (cons (car l)
                      ((insertR-f test?) new old (cdr l)))))))

(define (seqL old new l)
  (cons new (cons old l)))
(define (seqR old new l)
  (cons old (cons new l)))

(define (insertL-f test?)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (seqL old new (cdr l)))
          (else (cons (car l)
                      ((insertL-f test?) new old (cdr l)))))))

(define (insertR-f test?)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (cons old (cons new (cdr l))))
          (else (cons (car l)
                      ((insertR-f test?) new old (cdr l)))))))

(define (insert-g seq)
  (lambda (new old l)
    (cond ((null? l) '())
          ((eq? old (car l)) (seq old new (cdr l)))
          (else (cons (car l)
                      ((insert-g seq) new old (cdr l)))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(define insertL (insert-g
                 (lambda (old new l)
                   (cons new (cons old l)))))

(define subst
  (lambda (new old l)
    (cond ((null? l) '())
          ((eq? (car l) old)
           (cons new (cdr l)))
          (else (cons (car l)
                      (subst new old (cdr l)))))))

(define subst (insert-g
               (lambda (new l)
                 (cons new l))))

(define rember
  (lambda (a l)
    ((insert-g (lambda (old new l) l)) #f a l))) ; #f isnt actually used but we need it there because arity

(rember 'apple '(apple pie))
(newline)

(define ^
  (lambda (a b)
    (cond ((= b 0) 1) ; a^0 = 1
          (else (* a (^ a (- b 1)))))))

(^ 5 2)
(^ 5 1)
(^ 5 0)
(newline)

(define (atom-to-function x)
  (cond ((eq? x '+) +)
        ((eq? x '*) *)
        (else ^)))

(define operator
  (lambda (nexp)
    (car nexp)))
(define first-op
  (lambda (nexp)
    (car (cdr nexp))))
(define second-op
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(atom-to-function (operator '(+ 5 3)))
(newline)

(define value
  (lambda (nexp)
    (cond ((atom? a) a)
          (else ((atom-to-function (operator nexp)) (value (first-op nexp))
                                                    (value (second-op nexp)))))))

(define (multirember-f test?)
  (lambda (a l)
    (cond ((null? l) '())
          ((test? a (car l)) ((multirember-f test?) a (cdr l)))
          (else (cons (car l)
                      ((multirember-f test?) a (cdr l)))))))

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
(newline)

(define multirember-eq (multirember-f eq?))

(define eq?-tuna
  (lambda (x) (eq? 'tuna x)))

(define multirember-tuna (multirember-f eq?-tuna))

(define multiremberT
  (lambda (f lat)
    (cond ((null? lat) '())
          ((f (car lat)) (multiremberT f (cdr lat)))
          (else (cons (car lat) (multiremberT f (cdr lat)))))))

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))
(newline)

(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat) (col '() '()))
          ((eq? (car lat) a) (multirember&co a (cdr lat)
                                             (lambda (newlat seen)
                                               (col newlat (cons (car lat) seen)))))
          (else (multirember&co a (cdr lat) (lambda (newlat seen)
                                              (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend) ; #f
(multirember&co 'tuna '() (lambda (newlat seen) (a-friend newlat (cons 'tuna seen))))
(newline)

; uses a-friend on '() and '(tuna)
; returns #f since '(tuna) is not null
(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons 'tuna seen))))

(multirember&co 'tuna '(and tuna) a-friend)
(multirember&co 'tuna '(tuna) (lambda (newlat seen) (a-friend (cons 'and newlat) seen)))
(newline)

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat) seen)))

(define multiinstertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((equal? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
          (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multiinstertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((equal? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((equal? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
          ((equal? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
          (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((equal? (car lat) oldL) (cons new (cons oldL
                                                  (multiinsertLR&co new oldL oldR
                                                                    (cdr lat) (lambda (newlat Lin Rin)
                                                                                (col (cons new
                                                                                           (cons (car lat)
                                                                                                 newlat))
                                                                                     (+ Lin 1) Rin))))))
          
          ((equal? (car lat) oldR) (cons oldR (cons new
                                                    (multiinsertLR&co new oldL oldR
                                                                      (cdr lat) (lambda (newlat Lin Rin)
                                                                                  (col (cons (car lat)
                                                                                             (cons (car lat)
                                                                                                   newlat))
                                                                                       Lin (+ Rin 1)))))))
          
          (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat) (lambda (newlat Lin Rin)
                                                                         (col (cons (car lat) newlat) Lin Rin))))))))

(define evens-only*
  (lambda (lat)
    (cond ((null? lat) '())
          ((atom? (car lat)) (cond ((even? (car lat)) (cons (car lat) (evens-only* (cdr lat))))
                                   (else (evens-only* (cdr lat)))))
          (else (cons (evens-only* (car lat)) (evens-only* (cdr lat)))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define evens-only*&co
  (lambda (lat col)
    (cond ((null? lat) (col '() 1 0))
          ((atom? (car lat)) (cond ((even? (car lat)) (cons (car lat) (evens-only*&co (cdr lat) (lambda (l even odd)
                                                                                                  (col (cons (car lat) l)
                                                                                                   (* even (car lat)) odd)))))
                                   (else (evens-only*&co (cdr lat) (lambda (l even odd)
                                                                     (col l even (+ odd (car lat))))))))
          (else (cons (evens-only*&co (car lat) (lambda (l even odd) l even odd))
                      (evens-only*&co (cdr lat) (lambda (l even odd) l even odd)))))))