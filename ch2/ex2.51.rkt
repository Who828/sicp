(define (below-a painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           ((transform-painter (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)
            painter1))
          (paint-top
           ((transform-painter split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))
            painter2)))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (rotate-90 painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 1.0 1.0)
                      (make-vect 0.0 0.0))
   painter))

(define (rotate-270 painter)
     ((transform-painter (make-vect 0.0 1.0)
                         (make-vect 0.0 0.0)
                         (make-vect 1.0 1.0)
                         ) painter))
(define (below-b painter1 painter2)
  (rotate-90 (beside (rotate-270 painter1) (rotate-270 painter2))))