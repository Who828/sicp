(define (accumulate op initial sequnce)
  (if (null? sequnce)
      initial
      (op (car sequnce)
          (accumulate op initial (cdr sequnce)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))

(define (triple-pairs n)
  (flatmap (lambda (i)
             (flatmap (lambda (j) 
                        (map (lambda(k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (filter-triple-sum n s)
  (filter (lambda(x) (triple-sum? x s)) (triple-pairs n))) 

(define (triple-sum? seq s)
  (= (accumulate + 0 seq) s))

(define (make-triple-sum triple)
   (append triple (list (accumulate + 0 triple))))

(define (ordered-triple-sum n s)
   (map make-triple-sum
        (filter-triple-sum n s)))