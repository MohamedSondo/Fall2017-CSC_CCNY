(define (function-recur n)
        (cond ((< n 3) n)
              (else (+ (function-recur (- n 1)) 
                       (* 2 (function-recur (- n 2))) 
                       (* 3 (function-recur (- n 3)))))))

(define (function-iter n)
        (cond ((< n 3) n)
              (else 0))) ; replace this line with actual impl


(define (pascal row num)
        (cond ((or (<= num 1) (>= num row)) 1)
              (else (+ (pascal (- row 1) (- num 1)) 
                       (pascal (- row 1) num)))))

(define (sum-digits-recur num)
        (cond ((<= num 0) 0)
              ((< num 10) num)
              (else (+ (sum-digits-recur (quotient num 10)) 
                       (modulo num 10)))))

(define (sum-digits-iter num)
        (sum-digits-tail num num))

(define (sum-digits-tail num sum)
        (cond ((< num 10) sum))
              (else (sum-digits-tail (quotient num 10) 
                                     (+ sum (modulo num 10)))))

(define (increasing-recur num)
  (cond ((< num 100) (>= (modulo num 10) (quotient num 10)))
        (else (and (>= (modulo num 10) (modulo (quotient num 10) 10)) 
                   (increasing-recur (quotient num 10))))))

(define (increasing-iter num)
        (increasing-tail num 9))

(define (increasing-tail num digit)
        (cond ((> (modulo num 10) digit) #f)
              ((< num 10) #t)
              (else (increasing-tail (quotient num 10) 
                                     (modulo num 10)))))

(define (function-iter n)
  (cond ((< n 3) n)
  (else (function-tail n 2 2 1 0))))

(define (function-tail n count result a b)
  (cond ((= count n) result)
        (else (function-tail n (+ count 1) 
                             (+ result (* 2 a) (* 3 b)) result a))))
