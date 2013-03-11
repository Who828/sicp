(define (make-accumulator sum)
  (lambda (value)
    (begin (set! sum (+ sum value))
           sum)))
