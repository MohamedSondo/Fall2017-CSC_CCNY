(define (replace-nth lst n old new)
  (cond ((null? lst) '())
        ((pair? (car lst)) (cons (replace-nth (car lst) n old new) (replace-nth (cdr lst) n old new)))
        ((eq? (car lst) old) (if (= n 1) (cons new (cdr lst))
                               (cons (car lst) (replace-nth (cdr lst) (- n 1) old new))))
        (else (cons (car lst) (replace-nth (cdr lst) n old new)))))

(replace-nth '(1 (1 2 3) 1 3) 2 1 4)