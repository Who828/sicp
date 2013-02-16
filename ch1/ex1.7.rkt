(define (sqrt x)
      (define (good-enough? guess old-guess)
        (< (abs (- guess old-guess)) 0.001))
      (define (improve guess x) (average guess (/ x guess)))
      (define (sqrt-iter guess x old-guess)
        (if (good-enough? guess old-guess)
            guess
            (sqrt-iter (improve guess x) x guess)))
      (sqrt-iter 1.0 x 0))

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))