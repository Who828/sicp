(define (true-map proc seq)
  (if (null? seq)
      nil
      (let ((item (proc (car seq))))
           (if item
               (cons item (true-map proc (cdr seq)))
               #f))))

(define (apply-generic op . args) 
  (define (iter type-tags args) 
    (if (null? type-tags) 
        (error "No method for these types-ITER") 
        (let ((type1 (car type-tags))) 
          (let ((filtered-args (true-map (lambda (x) 
                                           (let ((type2 (type-tag x))) 
                                             (if (eq? type1 type2) 
                                                 x 
                                                 (let ((t2->t1 (get-coercion type2 type1))) 
                                                   (if (null? t2->t1) #f (t2->t1 x)))))) 
                                         args))) 
            (or filtered-args 
                (iter (cdr type-tags) args)))))) 
  (let ((type-tags (map type-tag args))) 
    (let ((proc (get op type-tags))) 
      (if (not (null? proc)) 
          (apply proc (map contents args)) 
          (apply apply-generic (cons op (iter type-tags args)))))))

