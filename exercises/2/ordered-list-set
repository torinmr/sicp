(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(assert (element-of-set? 3 '(1 2 3 4 5)))
(assert (not (element-of-set? 3 '(1 2 4 5))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(assert (equal? (intersection-set '(1 3 4 5) '(1 2 3 5)) '(1 3 5)))
(assert (equal? (intersection-set '() '(1 2 3 5)) '()))
(assert (equal? (intersection-set '(1 2 3 5) '()) '()))

;; adjoin-set in 61

;; union-set in 62
