
(define (make-queue)
 '(() . ()))

(define (queue-empty? q)
 (and (null? (car q)) (null? (cdr q))))

(define (queue-member? x q)
 (define (list-member? x lst)
   (if (null? lst) #f
       (if (= x (car lst)) #t
           (list-member? x (cdr lst)))))
 (or (list-member? x (car q))
     (list-member? x (cdr q))))

(define (queue-in x q)
 (cons (car q) (cons x (cdr q))))

(define (queue-out q)
 (if (null? (car q))
     (queue-out (cons (reverse (cdr q)) '()))
     (values (caar q) (cons (cdar q) (cdr q)))))


(define (make-pqueue)
 (make-tree))

(define (pqueue-empty? q)
  (tree-empty? q))

(define (pqueue-in p x q)
  (let ((y (tree-ref p q)))
   (if (null? y)
    (tree-set-splay p (queue-in x (make-queue)) q)
    (tree-set-splay p (queue-in x y) q))))

(define (pqueue-out q)
  (let ((y (tree-min q)))
    (call-with-values (lambda () (queue-out (tree-value y)))
                      (lambda (v yp)
                       (if (queue-empty? yp)
                           (values v (tree-remove-splay (tree-key y) q))
                           (values v (tree-set-splay (tree-key y) yp q)))))))

(define (pqueue-member? x q)
  (if (tree-empty? q) #f
      (not (null? (tree-find (lambda (k v) (queue-member? x v)) q)))))
