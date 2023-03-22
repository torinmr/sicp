(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; union-set is in 59

(define (set-eq? set1 set2)
  (and (every (lambda (x) (element-of-set? x set2)) set1)
       (every (lambda (x) (element-of-set? x set1)) set2)))

(assert (set-eq? '(1 2 3) '(1 2 3)))
(assert (set-eq? '(1 2 3) '(3 2 1)))
(assert (set-eq? '(1) '(1)))
(assert (set-eq? '() '()))
(assert (not (set-eq? '() '(1))))
(assert (not (set-eq? '(2) '(1))))
(assert (not (set-eq? '(2 1) '(1 2 3))))
(assert (not (set-eq? '(2 1 4) '(1 2 3))))
