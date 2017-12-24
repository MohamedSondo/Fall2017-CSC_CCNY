(define call/cc call-with-current-continuation)

(define (make-queue)
 (define p (cons '() '() ) )
 (cons p p))

(define (empty? q)
  (eq? (car (front q)) '() ))

(define (front q)
 (car q))

(define (rear q)
 (cdr q))

(define (push q e)
 (define p (cons e '()))
 (if (empty? q)
  (begin (set-car! q p)
   (set-cdr! q p))
  (begin
   (set-cdr! (rear q) p)
   (set-cdr! q p))))

(define (pop q)
 (define x 0)
 (if (empty? q)
  'empty
  (if (and (eq? (front q) (rear q))  (eq? '() (cdr (front q)))   )
   (begin
    (set! x (car (front q)))
    (set-car! (front q) '() )
    x) 
   (begin
    (set! x (car (front q)))
    (set-car! q (cdr (front q)) )
    x))))

(define process-queue (make-queue))

(define (coroutine thunk)
  (push process-queue thunk))

(define (start)
  ((pop process-queue)))

(define (pause)
  (call/cc
   (lambda (k)
     (coroutine (lambda () (k #f)))
     (start))))

;======================================

; Channel Definition
; Pair of a list and flag
; Methods:
; check-channel -> bool
; peak-channel -> list
; read-channel! -> list
; write-channel! -> channel

(define (make-channel name)
  ; channels are a dotted pair.
  ; (car (car(channel)) => channel name
  ; (car (cdr(channel)) => channel's value
  ; (cdr channel) => channel's set flag
  (cons (cons name '()) #f))

(define *channels* '())

(define (make-channel! chan-name)
  (let ((new-chan (cons (cons chan-name '() ) #f)))
    (set! *channels* (cons new-chan *channels*))
    new-chan))

(define (channel-set? chan-name)
  (let ((chan (get-channel *channels* chan-name))) (cdr chan)))

(define (get-channel chan-list chan-name)
  (cond ((null? chan-list) '())
        ((eq? (caaar chan-list) chan-name) (car chan-list))
        (else (get-channel (cdr chan-list) chan-name))))

(define (read-channel! chan-name)
  (if (not (channel-set? chan-name)) 
      (display "foo")
  (let* ((chan (get-channel *channels* chan-name)) (value (cdr (car chan))))
    (display chan)
    (set-car! chan (cons (caar chan) '()))
    (set-cdr! chan #f)
    value)))

(define (write-channel! chan-name value)
  (if (channel-set? chan-name) (
                          (display "Channel not set")))
  (let ((chan (get-channel *channels* chan-name)))
    (set-car! chan (cons (caar chan) value))
     (set-cdr! chan #t)))

(define (check-channel? chan)
  (cdr chan))

(define (peak-channel chan)
  (if (not (check-channel? chan)) (display "Channel not set"))
  (car chan))


;=======================================
; Coroutine testing
;
;(coroutine (lambda ()
;             (let loop ((i 0))
;               (if (< i 10)
;                   (begin
;                     (display (+ 1 i))
;                     (display " ")
;                     (pause)
;                     (loop (+ 1 i)))))))
;
;(coroutine (lambda ()
;             (let loop ((i 0))
;               (if (< i 10)
;                   (begin
;                     (display (integer->char (+ i 97)))
;                     (display " ")
;                     (pause)
;                     (loop (+ 1 i)))))))


;========================================
; Channel Testing

(define some-channel (make-channel 'foo))

;(coroutine (lambda ()
;             (let loop ( (i 0) (x '()))
;             (if (< i 10)
;                 (begin
;                   (if (not (check-channel? some-channel)) (pause))
;                   (set! x (read-channel! some-channel)) 
;                   (display "Channel Read: ")
;                   (display x)
;                   (display ";")
;                   )
;                   (loop (+ 1 i))))))

; Run (write-channel! some-channel 5)
;(write-channel! some-channel 5)
; prefix operator
; creates a process which executes some action before behaving like another process
(define (prefix event proc)
  (begin
    ; execute the passed event
    (event)
    (coroutine proc)))

(define (sayhi)
  (display "Hello event!")
  (newline))

(define (testproc)
  (display "Hello, world!")
  (newline))

(define (testpre) (prefix sayhi testproc))
(coroutine testpre)
(start)

; deterministic choice
(define (d-choice event1 sym1 proc1 event2 sym2 proc2 chan)
  (lambda ()
    (cond ((and (check-channel? chan) (eq? (car (peak-channel chan)) sym1))
           (coroutine (prefix event1 proc1)))
          ((and (check-channel? chan) (eq? (car (peak-channel chan)) sym2))
           (coroutine (prefix event2 proc2)))
          (else (coroutine (d-choice event1 sym1 proc1 event2 sym2 proc2 chan))))))

; random number generator seeded with length of process queue
(define num (length process-queue))
(define (rng)
  (define (nextnum n)
    ; values used by glibc
    (let ((a 1103515245) (c 12345) (m (expt 2 32)))
      (modulo (+ (* a n) c) m)))
  ;X(n+1) = (a*X(n)+c) mod m
  (set! num (nextnum num))
  num)

(modulo (rng) 10) ; number from 0-9
(+ (modulo (rng) 10) 1) ; from 1-10

(define (non-determ-choice event1 sym1 proc1 event2 sym2 proc2 chan)
  
  (define (phase2 sym ev1 pr1 ev2 pr2 chan)
    (lambda ()
      (cond ((and (check-channel? chan) (eq? (car (peak-channel chan)) sym))
             (if (= (modulo (rng) 2) 0)
                 (coroutine (prefix ev1 pr1))
                 (coroutine (prefix ev2 pr2))))
            (else (coroutine (non-determ-choice event1 sym1 proc1 event2 sym2 proc2 chan))))))
  
  (lambda ()
    (cond ((check-channel? chan) (cond ((eq? (car (peak-channel chan)) sym1)
                                        (coroutine (phase2 sym2 event1 proc1 event2 proc2 chan)))
                                       ((eq? (car (peak-channel chan)) sym2)
                                        (coroutine (phase2 sym1 event1 proc1 event2 proc2 chan)))
                                       (else (coroutine (non-determ-choice event1 sym1 proc1 event2 sym2 proc2 chan)))))
          (else (coroutine (non-determ-choice event1 sym1 proc1 event2 sym2 proc2 chan))))))
