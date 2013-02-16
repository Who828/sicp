(define (cons a b) 
  (* (expt 2 a)
        (expt 3 b)))
(define (num-of-divs a b)
  (if (= (remainder a b) 0)
      (+ 1  (num-of-divs (/ a b) b))
      0
      ))

(define (car l) (num-of-divs l 2))

(define (cdr l) (num-of-divs l 3))