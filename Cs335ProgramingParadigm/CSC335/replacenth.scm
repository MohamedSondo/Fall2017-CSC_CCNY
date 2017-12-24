 

; replace-nth

; Here is a tree recursion somewhat more complicated than those we have looked at until now


; develop and certify a scheme program replace-nth which takes as input

;        a list lst, not necessarily a list of atoms
;        a positive integer, n
;        an atom, old
;        an atom, new

; (replace-nth lst n old new) should replace the nth occurrence of old in 
; lst by new (and leave everything else unchanged)



; here is one idea -- can you explain what is going on?  can you show
; that the code is correct?  can you think of an alternative design?



(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(define count
  (lambda (tree a)
    (cond ((null? tree) 0)
          ((atom? (car tree))
           (cond ((eq? (car tree) a)
                  (+ 1 (count (cdr tree) a)))
                 (else (count (cdr tree) a))))
          (else
           (+ (count (car tree) a)
              (count (cdr tree) a))))))


(define replace-nth
  (lambda (tree old n new)
    (cond ((null? tree) tree)
          ((atom? (car tree))
           (cond ((eq? (car tree) old)
                  (cond ((= n 1) (cons new (cdr tree)))
                        (else (cons old (replace-nth (cdr tree) old (- n 1) new)))))
                 (else (cons (car tree) (replace-nth (cdr tree) old n new)))))
          (else
           (cond ((<= n (count (car tree) old))
                  (cons (replace-nth (car tree) old n new)
                        (cdr tree)))
                 (else
                  (cons (car tree)
                        (replace-nth (cdr tree) old (- n (count (car tree) old)) new))))))))

; some data

(define (make-bin-tree left-subtree right-subtree)
  (list left-subtree right-subtree))

(define (left-subtree bin-tree)
  (car bin-tree))

(define (right-subtree bin-tree)
  (cadr bin-tree))

(define (make-leaf a) a)


(define tree-010 (make-bin-tree (make-leaf 1) (make-leaf 2)))
(define tree-01 (make-bin-tree tree-010 (make-leaf 1)))
(define tree-0 (make-bin-tree (make-leaf 1) tree-01))

(define tree-1001 (make-bin-tree (make-leaf 2) (make-leaf 1)))
(define tree-100 (make-bin-tree (make-leaf 2) tree-1001))
(define tree-101 (make-bin-tree (make-leaf 1) (make-leaf 2)))
(define tree-10 (make-bin-tree tree-100 tree-101))

(define tree-111 (make-bin-tree (make-leaf 1) (make-leaf 2)))
(define tree-11 (make-bin-tree (make-leaf 1) tree-111))
(define tree-1 (make-bin-tree tree-10 tree-11))

(define tree (make-bin-tree tree-0 tree-1))


; suggested exercise: draw this tree and explain the notation


tree

(replace-nth tree 1 5 3)


; we can use let to avoid the second call (count (car tree) old) as follows:

(define replace-nth
  (lambda (tree old n new)
    (cond ((null? tree) tree)
          ((atom? (car tree))
           (cond ((eq? (car tree) old)
                  (cond ((= n 1) (cons new (cdr tree)))
                        (else (cons old (replace-nth (cdr tree) old (- n 1) new)))))
                 (else (cons (car tree) (replace-nth (cdr tree) old n new)))))
          (else
           (let ( (m (count (car tree) old))  )       
             (cond ((<= n m)
                    (cons (replace-nth (car tree) old n new)
                          (cdr tree)))
                   (else
                    (cons (car tree)
                          (replace-nth (cdr tree) old (- n m) new)))))
           ))))




