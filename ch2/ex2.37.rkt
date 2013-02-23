(define (dot-product v w)
     (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v) (map (lambda (x)
                                      (dot-product x v)) m))

(define (transpose mat) (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
     (let ((cols (transpose n)))
(map (lambda(x) 
       (matrix-*-vector cols x)) m)))

(define (accumulate-n op init seqs)
         (if (null? (car seqs))
nil
(cons (accumulate op init (map car seqs))
(accumulate-n op init (map cdr seqs)))))
 
 (define (accumulate op initial sequnce)
  (if (null? sequnce)
      initial
      (op (car sequnce)
          (accumulate op initial (cdr sequnce)))))