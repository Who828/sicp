(define (make-leaf symbol weight)
  (list `leaf symbol weight))
(define (leaf? object)
  (eq? (car object) `leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        `()
        (let ((next-branch
               (choose-branch
                (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      `()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf `A 4)
                  (make-code-tree
                   (make-leaf `B 2)
                   (make-code-tree (make-leaf `D 1)
                                   (make-leaf `C 1)))))
(define sample-message `(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (element-of-set? x set) 
  (cond ((null? set) false) 
        ((equal? x (car set)) true) 
        (else (element-of-set? x (cdr set))))) 

(define (encode message tree)
  (if (null? message)
      `()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (left-branch  tree) (car  tree))
(define (right-branch tree) (cadr tree))

(define (encode-symbol symbol tree)
  (define (correct-branch? branch)
    (if (leaf? branch)
        (equal? symbol (symbol-leaf branch))
        (element-of-set? symbol (symbols branch))))
  
  (let ((lb (left-branch tree))
        (rb (right-branch tree)))
    (cond ((correct-branch? lb) 
           (if (leaf? lb) '(0) (cons 0 (encode-symbol symbol lb)))) 
          ((correct-branch? rb) 
           (if (leaf? rb) '(1) (cons 1 (encode-symbol symbol rb)))) 
          (else (error "bad symbol -- ENCODE-SYMBOL" bit)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (define (successive-merge-iter leaf-set tree)
    (if (null? leaf-set)
        tree
        (successive-merge-iter (cdr leaf-set) (make-code-tree (car leaf-set) tree))))
  (successive-merge-iter (cdr leaf-set) (car leaf-set)))