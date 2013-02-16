(define (square x) (* x x))

(define (sum-of-square-largest-numbers x y z)
  (+ (square (max x y)) (square (max (min x y) z) )))
  
(define (max x y) 
  (if (> x y)x y))

(define (min x y)
  (if (> x y) y x))
