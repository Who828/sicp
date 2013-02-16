(define (make-point x y)
    (cons x y))

(define (x-point l) (car l))

(define (y-point l) (cdr l))

(define (make-segment x y) (cons x y))

(define (start-segment l) (car l))

(define (end-segment l) (cdr l))

(define (print-point p)
   (newline)
   (display "(")
   (display (x-point p))
   (display ",")
   (display (y-point p))
   (display ")"))

(define (midpoint-segment x) (make-point 
                              (/ (+ (x-point (start-segment x)) (x-point (end-segment x))) 2)
                              (/ (+ (y-point (start-segment x)) (y-point (end-segment x))) 2)))

(define (make-rect a b) (cons a b))

(define (rect-width l) (car l))

(define (rect-height l) (cdr l))

(define (rect-area l) (* (car l) (cdr l)))

(define (rect-perimeter l) (* 2 (+ (car l) (cdr l))))
