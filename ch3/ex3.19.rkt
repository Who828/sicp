(define (cycle? x)
  (define (cycle-iter z fast-z)
    (cond ((null? fast-z) #f) 
          ((eq? (car z) (car fast-z))
           #t)
          (else (cycle-iter (cdr z) (cddr fast-z)))))
  (cycle-iter z (cdr z)))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))