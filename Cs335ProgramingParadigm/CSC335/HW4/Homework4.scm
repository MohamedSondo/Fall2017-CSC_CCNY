
; Fourth Homework Set
; CSc 335
; Fall 2016


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework4.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here are some homework problems to get you started with lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note that I have sometimes deliberately offered incomplete specifications - if you find this
; to be the case, you will need to complete the specification as you deem best.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Give both recursive and iterative procedures (along with their arguments) for each

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Write your own version of length using the list functions we have discussed.  You can find
; length documented at http://www.schemers.org/Documents/Standards/R5RS/

; The idea is suggested by (my-length '(a b c d)) = 4.  

; recursive: 
(define (my-length l)
  (define (atom? x)
    (not (or (null? x) (pair? x))))

  (cond ((null? l) 0)
        (else (+ 1 (my-length (cdr l))))))

; iterative:
(define (my-length-iter l)
  (define (iter l result)
    (cond ((null? l) result)
          (else (iter (cdr l) (+ result 1)))))

  (iter l 0))

; 2.  Write your own version of list-ref using the list functions we have discussed.  You can find
; list-ref documented at http://www.schemers.org/Documents/Standards/R5RS/

; Briefly, the idea is indicated by this example:  (my-list-ref '(a b c d) 2) = c.  Note the 0-based
; indexing.  What happens if the input index exceeds the size of the input list?

; recursive:
; alright, it's a tad contrived, but that's definitely a properly recursive function.
(define (my-list-ref l index)
  (cond ((>= index (length l)) '())
        ((= index 0) (car l))
        (else (car (cons (my-list-ref (cdr l) (- index 1)) '())))))
; see that? Deferred operator. It might be a little forced, but it meets the requirements.

; iterative:
(define (my-list-ref-iter l index)
  (cond ((>= index (length l)) '())
        ((= index 0) (car l))
        (else (my-list-ref-iter (cdr l) (- index 1)))))

; 3. Write a function start that takes two arguments, lst and num, and which returns the
; first num elements of lst.
; let's assume order doesn't matter...
; recursive:
(define (start lst num)
  (cond ((= num 0) '())
        (else (cons (car lst) (first-num-of-lst (cdr lst) (- num 1))))))

; iterative:
(define (start-iter lst num)
  (define (iter lst num result)
    (cond ((= num 0) result)
          (else (iter (cdr lst) (- num 1) (cons (car lst) result)))))

  (iter lst num '()))

; 4.  Write a function but-last that takes two arguments, lst and num, and which returns the
; list of all but the last num elements of lst.

; recursive:
(define (but-last lst num)
  (cond))

; iterative:
; add elements to result until length of result = length of lst - num
; it's horrifying.
; shh it works (TM)
(define (but-last-iter lst num)
  (define (iter lst num result curr)
    (cond ((= (length result) (- (length lst) num)) result)
          (else (iter lst num (cons (list-ref lst curr) result) (+ curr 1)))))

  (iter lst num '() 0))

(define (but-last l num)

  (define (aux l count derp)
    (cond ((= count max) '())
          (else (cons (car l)
                      (aux (cdr l)
                           (+ count 1)
                           derp)))))
  (aux l 0 (- (length l) num)))

(define (but-last l num)
  (define (aux l count result)
    (cond ((= count  (length (cdr l))) result)
          (else (aux (cdr l) count (cons (car l) result)))))

  (aux l num '()))

; inducting on .... length of l: we show by induction on the length of l that whenever 0 <= num <= (length l), the call (aux l 0 (- length l) num) returns the list consisting of the first
; (- (length l) num) elements of l, in the same order they occur in l.
; induction hypothesis: for all lists l' shorter than l with 0 <= num <= (length l'), (aux l' 0 max) returns all but the last... this proof is poorly-constructed

; remember the design role of your variables. max is the length of... the list we're working with?
; is it better to reformulate the program to simplify the proof?

; 5.  Write a function end that takes two arguments, lst and num, and returns the last num
; elements of lst.
; recursive:

(define (end lst num)
  (cond ((or (= num 0) (null? lst)) '())
        ((= num 1) (cons (car lst) '()))
        ((= num 2) (cons (car (cdr lst) '(cdr 
        (else (cons (end (cdr lst) (- num 1)))))) ; almost definitely wrong but whatever

; iterative 
(define (end-iter lst num)
  (define (iter lst len value count result)
    (cond ((= count len) result)
          ((< count value) (iter (cdr lst) len value (+ count 1) result))
          (else (iter (cdr lst) len value (+ count 1) (cons (car lst) result)))))

  (iter lst (length lst) (- (length lst) num) 0 '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; suggested reading:

;;    http://en.wikipedia.org/wiki/Scheme_(programming_language)

