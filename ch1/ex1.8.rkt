(define (cube-root x)
      (define (good-enough? guess old-guess)
        (< (abs (- guess old-guess)) 0.001))
      (define (improve guess) (newton-method guess x))
      (define (cube-root-iter guess x old-guess)
        (if (good-enough? guess old-guess)
            guess
            (cube-root-iter (improve guess) x guess)))
      (cube-root-iter 1.0 x 0))

(define (newton-method x y)
  (/ (+ (/ x (square y)) (* 2 y))) 3)
   
(define (square x) (* x x))