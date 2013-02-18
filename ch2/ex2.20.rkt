(define (same-parity a . b) 
  (if (even? a)
      (even-numbers (cons a b))
      (odd-numbers (cons a b))))

(define (even-numbers l)
  (cond ((null? l) (list))
    ((even? (car l))
      (cons (car l) (even-numbers (cdr l))))
      (else (even-numbers (cdr l)))))

(define (odd-numbers l)
  (cond ((null? l) (list))
    ((odd? (car l))
      (cons (car l) (odd-numbers (cdr l))))
      (else (odd-numbers (cdr l)))))

(define (odd? a) 
  (> (remainder a 2) 0))

(define (even? a) 
  (= (remainder a 2) 0))