(define (make-monitored f)
  (let ((times-called 0))
    (define(mf value)
      (set! times-called (+ times-called 1))
      (f value))
    
    (define (dispatch m)
      (cond ((eq? m `how-many-calls?) times-called)
      (else (mf m))))
  
  dispatch))

