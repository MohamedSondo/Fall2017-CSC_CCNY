(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))
; zero? is built in

(define sum
  (lambda (a b)
    (cond ((zero? b) a)
          (else (sum (add1 a) (sub1 b))))))

(define diff
  (lambda (a b)
    (cond ((zero? b) a)
          (else (diff (sub1 a) (sub1 b))))))

(define (addtup tup)
  (cond ((null? tup) 0)
        (else (sum (car tup) (addtup (cdr tup))))))

(define (product a b)
  (cond ((zero? b) 0)
        ((= b 1) a)
        (else (sum a (product a (sub1 b))))))

(product 5 3)

(define (tup+ tup1 tup2)
  (cond ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else (cons (sum (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))

(tup+ '(1 2 3 4 5) '(9 8 7 6 5))
(tup+ '(1 2 3 4 5) '(9 8 7 6 5 10 10 10))
(tup+ '(1 2 3 4 5 10 10 10) '(9 8 7 6 5))
(newline)
(define (gt a b)
  ; if a gets to zero first, > a b is false
  (cond ((zero? a) #f)
        ((zero? b) #t)
        (else (gt (sub1 a) (sub1 b)))))

(gt 5 10)
(gt 10 5)
(gt 100 0)
(gt 10 10)
(newline)

(define (lt a b)
  ; yes, you're very clever.
  (not (or (= a b) (gt a b))))

(lt 5 10)
(lt 10 5)
(lt 10 10)
(newline)

(define (other-lt a b)
  (cond ((zero? b) #f)
        ((zero? a) #t)
        (else (other-lt (sub1 a) (sub1 b)))))

(other-lt 5 10)
(other-lt 10 5)
(other-lt 10 10)
(newline)

(define (eq a b)
  (not (or (gt a b) (lt a b))))

(eq 10 10)
(eq 0 0)
(eq 10 5)
(eq 5 10)
(newline)

(define (pow a b)
  (cond ((zero? b) 1)
        (else (product a (pow a (sub1 b))))))

(define (pow-iter a b)
  (define (iter a b result)
    (cond ((zero? b) result)
          (else (iter a (sub1 b) (product result a)))))

  (iter a b 1))

(pow 1 1)
(pow 2 3)
(pow 5 3)
(newline)
(pow-iter 1 1)
(pow-iter 2 3)
(pow-iter 5 3)
(newline)

(define (my-length lat)
  (cond ((null? lat) 0)
        (else (add1 (my-length (cdr lat))))))

(define (my-length-iter lat)
  (define (iter lat result)
    (cond ((null? lat) result)
          (else (iter (cdr lat) (+ result 1)))))

  (iter lat 0))

(my-length '(hotdogs with mustard sauerkraut and pickles))
(my-length-iter '(hotdogs with mustard sauerkraut and pickles))

(my-length '(ham and cheese on rye))
(my-length-iter '(ham and cheese on rye))
(newline)

(define (pick n lat)
  (cond ((= n 1) (car lat))
        (else (pick (- n 1) (cdr lat)))))

(define (rempick n lat)
  (cond ((zero? (sub1 n)) (cdr lat))
        (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(rempick 3 '(hotdogs with hot mustard))
(newline)

(define (no-nums lat)
  (cond ((null? lat) '())
        ((number? (car lat)) (no-nums (cdr lat)))
        (else (cons (car lat) (no-nums (cdr lat))))))

(no-nums '(5 pears 6 prunes 9 dates))
(newline)

(define (all-nums lat)
  (cond ((null? lat) '())
        ((not (number? (car lat))) (all-nums (cdr lat)))
        (else (cons (car lat) (all-nums (cdr lat))))))

(all-nums '(5 pears 6 prunes 9 dates))
(newline)

(define (eqan? a1 a2)
  (cond ((and (number? a1) (number? a2)) (= a1 a2))
        (else (eq? a1 a2))))

(define (occurs a lat)
  (cond ((null? lat) 0)
        (else (cond ((eqan? a (car lat)) (add1 (occurs a (cdr lat))))
                    (else (occurs a (cdr lat)))))))

(occurs 5 '(1 2 3 4 5 4 3 2 1 2 3 4 5))

(define (one? n)
  (= n 1))

(define (rempick-one n lat)
  (cond ((one? n) (cdr lat))
        (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

