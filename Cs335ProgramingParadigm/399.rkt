(define (LargestIntegerSmallerThanNumWhichDoesNotContainDigits num digit1 digit2 digit3)
  (display num)
  (newline)
  (define (isvalid? n d1 d2 d3)
    (let ((dig (modulo n 10)))
      (cond ((or (= dig d1) (= dig d2) (= dig d3)) #f)
            ((< n 10) #t)
            (else (isvalid? (quotient n 10) d1 d2 d3)))))

  (cond ((isvalid? num digit1 digit2 digit3) num)
        (else (LargestIntegerSmallerThanNumWhichDoesNotContainDigits (- num 1) digit1 digit2 digit3))))

(LargestIntegerSmallerThanNumWhichDoesNotContainDigits 450 1 4 7)