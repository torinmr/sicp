(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(assert (= 1 (left-branch (make-tree 2 1 3))))
(assert (= 3 (right-branch (make-tree 2 1 3))))
(assert (= 2 (entry (make-tree 2 1 3))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(assert (element-of-set? 1 '(1 () ())))
(assert (element-of-set? 1 '(2
                             (1 () ())
                             (3 () ()))))
(assert (element-of-set? 3 '(2
                             (1 () ())
                             (3 () ()))))
(assert (not (element-of-set? 4 '(2
                                  (1 () ())
                                  (3 () ())))))
(assert (not (element-of-set? 0 '(2
                                  (1 () ())
                                  (3 () ())))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(assert (equal? (adjoin-set 1 '())
                '(1 () ())))
(assert (equal? (adjoin-set 1 '(2 () ()))
                '(2 (1 () ()) ())))
(assert (equal? (adjoin-set 3 '(2 () ()))
                '(2 () (3 () ()))))
(assert (equal? (adjoin-set 3 '(2
                                (1 () ())
                                (4 () ())))
                '(2
                  (1 () ())
                  (4 (3 () ()) ()))))
