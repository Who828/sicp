(define (reverse l)
  (if (null? l) l
      (cons (reverse (cdr l)) (car l))))

(define (deep-reverse l)
  (cond ((null? l) l)
        ((not (pair? (car l))) (reverse l))
      (else (cons (deep-reverse (cdr l)) (deep-reverse(car l))))))