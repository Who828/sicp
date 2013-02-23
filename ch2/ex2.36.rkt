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