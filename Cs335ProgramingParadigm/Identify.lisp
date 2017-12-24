Identify 

(define-monad
   <id>
   (lambda (a) a)
   (lambda (a f) (f a)))

(define-monad
   <maybe>
   (lambda (a) a)
   (lambda (a f) (if a (f (cadr a)) #f))
   (case-lambda (() 'Nothing)
                ((_ . _) 'Nothing)))
(return monad value)
Allows any function to lift a value into the monad provided. Ie,

(define (foo bar)
  (if (not bar)
      (fail <maybe>)
      (return <maybe> bar)))
Would be the same as:

(define (foo bar)
  (if (not bar)
      (<maybe>-fail)
      (<maybe>-unit bar)))
Would be the same as:

(define (foo bar)
  (if (not bar)
      'Nothing
      `(Just ,bar)))
