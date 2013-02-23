(define (count-leaves t)
(accumulate + 0 (map 
                 (lambda(x) 1) (enumerate-tree t))))


 (define (enumerate-tree tree)
      (cond ((null? tree) nil)
            ((not (pair? tree)) (list tree))
            (else (append (enumerate-tree (car tree))
                          (enumerate-tree (cdr tree))))))
 
 
(define (accumulate op initial sequnce)
  (if (null? sequnce)
      initial
      (op (car sequnce)
          (accumulate op initial (cdr sequnce)))))