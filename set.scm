(define (make-set)
  (make-tree))

(define (set-node x l r)
  (cons x (cons l r)))

(define (set-key set)
  (car set))

(define (set-left set)
  (cadr set))

(define (set-right set)
  (cddr set))

(define (set-rot-right set)
  (let ((l (set-left set)))
    (set-node (set-key l)
              (set-left l)
              (set-node (set-key set) (set-right l) (set-right set)))))

(define (set-rot-left set)
  (let ((r (set-right set)))
    (set-node (set-key r)
              (set-node (set-key set) (set-left set) (set-left r))
              (set-right r))))

(define (set-splay x set)
  (let ((k (set-key set))
        (l (set-left set))
        (r (set-right set)))
    (if (< x k)
        (cond
         ((= x (set-key l))
          (set-rot-right set))
         ((and (< x (set-key l)) (= x (set-key (set-left l))))
          (set-rot-right (set-rot-right set)))
         ((and (> x (set-key l)) (= x (set-key (set-right l))))
          (set-rot-right (set-node k (set-rot-left l) r)))
         (else
          (set-splay x (set-node k (set-splay x l) r))))
        (cond
         ((= x k) set)
         ((= x (set-key r))
          (set-rot-left set))
         ((and (> x (set-key r)) (= x (set-key (set-right r))))
          (set-rot-left (set-rot-left set)))
         ((and (< x (set-key r)) (= x (set-key (set-left r))))
          (set-rot-left (set-node k l (set-rot-right r))))
         (else
          (set-splay x (set-node k l (set-splay x r))))))))

(define (set-insert x set)
  (if (null? set)
      (set-node x '() '())
      (let ((k (set-key set)))
        (if (= x k) set
            (if (< x k)
                (set-node k (set-insert x (set-left set)) (set-right set))
                (set-node k (set-left set) (set-insert x (set-right set))))))))

(define (set-add x set)
  (set-splay x (set-insert x set)))

(define (set-member? x set)
    (if (null? set) #f
        (let ((k (set-key set)))
          (if (= x k) #t
              (if (< x k)
                  (set-member? x (set-left set))
                  (set-member? x (set-right set)))))))