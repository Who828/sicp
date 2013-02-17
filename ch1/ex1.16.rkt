(define (fast-expt b n)
      (cond ((= n 0) 1)
            ((even? n) (square (fast-expt b (/ n 2))))
            (else (* b (fast-expt b (- n 1))))))

(define (even? n)
      (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fast-expt-iter b n)
  (iter-loop 1 b n))

(define (iter-loop a b n)
   (cond ((= n 1) a)
            ((even? n) (iter-loop (* a (square b)) b (/ n 2)))
            (else (iter-loop (* a b) b (- n 1)))))