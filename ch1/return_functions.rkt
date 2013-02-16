(define (deriv g)

(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

(define (newton-transform g)

(lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)

(fixed-point (newton-transform g) guess))
(define tolerance 0.00001)

(define (fixed-point f first-guess)

(define (close-enough? v1 v2)

(< (abs (- v1 v2))

tolerance))

(define (try guess)

(let ((next (f guess)))

(if (close-enough? guess next)

next

(try next))))

(try first-guess))

(define (double g)
    (lambda (x)
      (g (g x))))

(define (compose f g)
    (lambda (x) (f (g x)) ))

(define (repeated f x)
    (if (= x 1) f
        (compose f (repeated f (- x 1)))))

(define (square x) ( * x x))