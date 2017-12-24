(define (rember* a l)
  (cond ((null? l) l)
        ((pair? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
        (else (if (eq? a (car l)) (rember* a (cdr l))
                                  (cons (car l) (rember* a (cdr l)))))))

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
(newline)

(define (insertR* new old l)
  (cond ((null? l) l)
        ((pair? (car l)) (cons (insertR* new old (car l))
                               (insertR* new old (cdr l))))
        (else (if (eq? old (car l)) (cons (car l) (cons new (insertR* new old (cdr l))))
                                    (cons (car l) (insertR* new old (cdr l)))))))

(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
(newline)

(define (occur* a l)
  (cond ((null? l) 0)
        ((pair? (car l)) (+ (occur* a (car l)) (occur* a (cdr l))))
        (else (cond ((eq? a (car l)) (+ 1 (occur* a (cdr l))))
                    (else (occur* a (cdr l)))))))

(occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
(newline)

(define (subst* new old l)
  (cond ((null? l) l)
        ((pair? (car l)) (cons (subst* new old (car l)) (subst* new old (cdr l))))
        (else (cond ((eq? old (car l)) (cons new (subst* new old (cdr l))))
                    (else (cons (car l) (subst* new old (cdr l))))))))

(subst* 'orange 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
(newline)

(define (insertL* new old l)
  (cond ((null? l) l)
        ((pair? (car l)) (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
        (else (cond ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
                    (else (cons (car l) (insertL* new old (cdr l))))))))

(insertL* 'pecker 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
(newline)

(define (member* a l)
  (cond ((null? l) #f)
        ((pair? (car l)) (or (member* a (car l)) (member* a (cdr l))))
        (else (or (eq? a (car l)) (member* a (cdr l))))))

(member* 'chips '((potato) (chips ((with) fish) (chips))))
(newline)

(define (leftmost l)
  (cond ((pair? l) (leftmost (car l)))
        (else l)))

(leftmost '(((hot) (tuna (and))) cheese))
(newline)

(define (eqlist? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ; if we get here one of them is null while the other isn't
        ((or (null? l1) (null? l2)) #f)
        ((and (pair? (car l1)) (pair? (car l2))) (and (eqlist? (car l1) (car l2))
                                                      (eqlist? (cdr l1) (cdr l2))))
        ; see the argument for null above
        ((or (pair? (car l1)) (pair? (car l2))) #f)
        ; if we get here
        (else (and (eq? (car l1) (car l2))
                   (eqlist? (cdr l1) (cdr l2))))))

(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))

(define (my-equal? s1 s2)
  (cond ((and (pair? s1) (pair? s2)) (eqlist? s1 s2))
        ((or (pair? s1) (pair? s2)) #f)
        (else (eq? s1 s2))))
