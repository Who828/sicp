(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (= type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (raise-types a1 a2))
                
                (error "No method for these types"
                       (list op type-tags))))))))


(define (raise-types a1 a2)
  (cond ((raise-into? a1)
         (apply-generic (raise a1) a2))
        ((raise-into? a2)(apply-generic a1 (raise a2)))
        ))

(define (raise-into? a)
  (let ((a-type (type-tag a)))
    (get 'raise (list a-type))))

