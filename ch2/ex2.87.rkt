(define global-array `())

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

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) `scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (square x) (* x x))

(define (add x y) (apply-generic `add x y))
(define (sub x y) (apply-generic `sub x y))
(define (mul x y) (apply-generic `mul x y))
(define (div x y) (apply-generic `div x y))
(define (equ? x y) (apply-generic `equ? x y))
(define (=zero? x) (apply-generic `=zero? x))




(define (install-scheme-number-package)
  (define (tag x) (attach-tag `scheme-number x))
  (put `add `(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put `sub `(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put `mul `(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put `div `(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put `equ? `(scheme-number scheme-number)
       (lambda (x y) (tag (= x y))))
  (put `make `scheme-number (lambda (x) (tag x)))
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  `done)

(define (make-scheme-number n)
  ((get `make `scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag `rational x))
  (put `add `(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put `sub `(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put `mul `(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put `div `(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put `equ? `(rational rational)
       (lambda (x y) (tag (equ? x y))))
  (put `make `rational
       (lambda (n d) (tag (make-rat n d))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  `done)
(define (make-rational n d)
  ((get `make `rational) n d))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list)) 
  (define (variable p) (car p)) 
  (define (term-list p) (cdr p))
  (define (variable? p) (symbol? p))
  (define (same-variable? p1 p2) (if (and (variable? p1) (variable? p2)) (eq? p1 p2) #f))
  
  
  (define (the-empty-termlist) ’())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (zero-poly? poly)
    (define (zero-term? term-list)
      (or (empty-termlist? term-list)
          (and (=zero? (coeff (first-term term-list)))
               (zero-term? (rest-terms term-list)))))
    (zero-term? (term-list poly)))
  
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var: ADD-POLY"
               (list p1 p2))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var: MUL-POLY"
               (list p1 p2))))
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (tag p) (attach-tag `polynomial p))
  (put `add `(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put `mul `(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put `make `polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  `done)

(define (make-polynomial var terms)
  ((get `make `polynomial) var terms))