(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square x) (* x x))

 (define (map proc items)
      (if (null? items)
          nil
          (cons (proc (car items))
                (map proc (cdr items)))))
 
 (define (square-list-map items)
   (map (lambda(x) (square x)) items)) 