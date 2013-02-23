(define (fold-left op initial sequence)
     (define (iter result rest)
       (if (null? rest)
           result
           (iter (op result (car rest))
                 (cdr rest))))
     (iter initial sequence))


(define (fold-right op initial sequnce)
  (if (null? sequnce)
      initial
      (op (car sequnce)
          (fold-right op initial (cdr sequnce)))))