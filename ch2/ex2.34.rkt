(define (horner-eval x coefficient-sequence) 
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
                     0
                     coefficient-sequence))


(define (accumulate op initial sequnce)
  (if (null? sequnce)
      initial
      (op (car sequnce)
          (accumulate op initial (cdr sequnce)))))