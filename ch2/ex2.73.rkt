(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (install-deriv-package)
  ;; internal procedures
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))  (define (make-from-real-imag x y) (cons x y))
  
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list `+ a1 a2))))
  
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list `* m1 m2))))
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  
  (define (deriv-product exp var)
    (make-sum (make-product
               (multiplier exp)
               (deriv (multiplicand exp) var))
              (make-product
               (deriv (multiplier exp) var)
               (multiplicand exp))))
  
  (define (product? x) (and (pair? x) (eq? (car x) `*)))
  (define (sum? x) (and (pair? x) (eq? (car x) `+)))
  
  (define (exponentation-deriv expr var) 
    (make-product (exponent expr) 
                  (make-product  
                   (make-exponentiation (base expr) 
                                        (make-sum (exponent expr) -1)) 
                   (deriv (base expr) var)))) 
  (define (exponent expr) 
    (cadr expr)) 
  (define (base expr) 
    (car expr)) 
  (define (make-exponentiation base exponent) 
    (cond ((=number? exponent 0) 1) 
          ((=number? exponent 1) base) 
          ((=number? base 1) 1) 
          (else (list '** base exponent)))) 
  
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum) 
  (put 'deriv '* deriv-product)
  (put 'deriv '** exponentation-deriv) 
  `done) 


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get `deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

