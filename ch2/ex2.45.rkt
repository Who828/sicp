(define (split dir1 dir2)
  (lambda(painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split dir1 dir2) painter (- n 1))))
          (dir1 painter (dir2 smaller smaller))))))

(define right-split (split beside below))

(define up-split (split below beside))