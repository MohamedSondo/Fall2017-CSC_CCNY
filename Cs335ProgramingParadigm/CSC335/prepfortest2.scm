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


