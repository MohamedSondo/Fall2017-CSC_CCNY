(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (nextx x) (+ x 1))
  (define (termx x) x)
  (product termx 1 nextx n))
