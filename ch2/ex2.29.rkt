(define (make-mobile left right) (cons left right))
         

(define (make-branch length structure)
           (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (cond ((not (pair? (branch-structure branch))) (branch-structure branch))
        (else (+ (total-weight (branch-structure branch))))))

(define (balanced? mobile)
  (= (branch-torque (left-branch mobile))
     (branch-torque (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))