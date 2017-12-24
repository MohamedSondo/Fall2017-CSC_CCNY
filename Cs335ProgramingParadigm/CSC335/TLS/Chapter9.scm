(define atom?
  (lambda (x) (not (or (null? x) (pair? x)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define pick
  (lambda (num l)
    (cond ((= num 1) (car l))
          (else (pick (- num 1) (cdr l))))))

(define keep-looking
  ; sorn = symbol or number
  (lambda (a sorn lat)
    (cond ((number? sorn) (keep-looking a (pick sorn lat) lat))
          (else (equal? sorn a)))))

(define eternity
  (lambda (x) (eternity x)))

(define (first l)
  (car l))
(define (second l)
  (car (cdr l)))

(define (build first second)
  (cons first (cons second '())))

;"The function shift takes a pair whose first
;component is a pair and builds a pair by
;shifting the second part of the first
;component into the second component."
(define shift
  (lambda (l)
    (build (first (first l))
           (build (second (first l)) (second l)))))

(shift '((a b) (c d)))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))