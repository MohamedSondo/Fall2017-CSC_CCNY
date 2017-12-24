(define (function-iter n)
  (cond ((< n 3) n)
  (else (function-tail n 2 2 1 0))))

(define (function-tail n count result a b)
  (cond ((= count n) result)
        (else (function-tail n (+ count 1) (+ result (* 2 a) (* 3 b)) result a))))

(function-iter 3)
(function-iter 4)
(function-iter 5)
(function-iter 6)

(define (sum-digits-iter num)
        (sum-digits-tail num 0))

(define (sum-digits-tail num sum)
        (cond ((< num 10) (+ sum num))
              (else (sum-digits-tail (quotient num 10) 
                                     (+ sum (modulo num 10))))))

(sum-digits-iter 12)
(sum-digits-iter 345)