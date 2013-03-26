(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  
  (let ((local-table (list `*table*)))
    
    (define (lookup key-list)
      (define (lookup-iter key-list table)
        (let ((subtable (assoc (car key-list) (cdr table))))
          (if subtable
              (if (null? (cdr key-list))
                  (cdr subtable)
                  (lookup-iter (cdr key-list) subtable))
              #f
              )))
      (lookup-iter key-list local-table))
    
    (define (insert! key-list value)
      (define (make-entry key-list)
        (if (null? (cdr key-list))
            (cons (car key-list) value)
            (list (car key-list) (make-entry (cdr key-list)))
            ))
      (define (insert-iter! key-list table)
        (let ((subtable
               (assoc (car key-list) (cdr table))))
          (if subtable
              (if (null? (cdr key-list))
                  (set-cdr! subtable value)
                  (insert-iter! (cdr key-list) subtable))
              (set-cdr! table (cons (make-entry key-list) (cdr table))))))
      (insert-iter! key-list local-table)
      `ok)
    
    (define (dispatch m)
      (cond ((eq? m `lookup-proc) lookup)
            ((eq? m `insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table (lambda(x y) (eq? x y))))
(define get (operation-table `lookup-proc))
(define put (operation-table `insert-proc!))


