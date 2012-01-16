(define (make-tree)
  '())

(define (tree-node k v . children)
  (if (null? children)
      (cons (cons k v) (cons '() '()))
      (cons (cons k v) (cons (car children) (cadr children)))))

(define (tree-key n)
  (caar n))

(define (tree-value n)
  (cdar n))

(define (tree-left n)
  (cadr n))

(define (tree-right n)
  (cddr n))

(define (tree-empty? n)
  (null? n))

(define (tree-leaf? n)
  (and (pair? n) (null? (tree-left n)) (null? (tree-right n))))

(define-syntax with-tree-node
  (syntax-rules ()
    ((_ (<tree> <key> <value> <left> <right>) <body> ...)
     (let ((<key> (tree-key <tree>))
           (<value> (tree-value <tree>))
           (<left>  (tree-left <tree>))
           (<right> (tree-right <tree>)))
       <body> ...))))

(define (tree-min tree)
  (if (tree-empty? tree) '()
      (if (or (tree-leaf? tree)
              (tree-empty? (tree-left tree)))
          tree
          (tree-min (tree-left tree)))))

(define (tree-max tree)
  (if (tree-empty? tree) '()
      (if (or (tree-leaf? tree)
              (tree-empty? (tree-right tree)))
          tree
          (tree-max (tree-right tree)))))

(define (tree-predecessor n)
  (if (or (tree-leaf? n) (tree-empty? (tree-left n))) '()
      (let loop ((m (tree-left n)))
        (if (tree-leaf? m) m
            (loop (tree-right m))))))

(define (tree-successor n)
  (if (or (tree-leaf? n) (tree-empty? (tree-right n))) '()
      (let loop ((m (tree-right n)))
        (if (tree-leaf? m) m
            (loop (tree-left m))))))

(define (tree-ref x tree)
  (if (tree-empty? tree) '()
      (with-tree-node (tree k v l r)
        (if (= x k) v
            (if (< x k)
                (tree-ref x l)
                (tree-ref x r))))))

(define (tree-set x y tree)
  (if (tree-empty? tree) (tree-node x y)
      (with-tree-node (tree k v l r)
        (if (= x k)
            (tree-node x y l r)
            (if (< x k)
                (tree-node k v (tree-set x y l) r)
                (tree-node k v l (tree-set x y r)))))))

(define (tree-insert x y tree)
  (if (tree-empty? tree) (tree-node x y)
      (with-tree-node (tree k v l r)
        (if (= x k) tree
            (if (< x k)
                (tree-node k v (tree-insert x y l) r)
                (tree-node k v l (tree-insert x y r)))))))

(define (tree-remove x tree)
  (if (tree-empty? tree) tree
      (with-tree-node (tree k v l r)
        (if (not (= x k))
            (if (< x k)
                (tree-node k v (tree-remove x l) r)
                (tree-node k v l (tree-remove x r)))
            (cond
             ((tree-leaf? tree) '())
             ((tree-empty? (tree-left tree)) (tree-right tree))
             ((tree-empty? (tree-right tree)) (tree-left tree))
             (else
              (with-tree-node ((tree-predecessor tree) pk pv pl pr)
                (tree-node pk pv (tree-remove pk l) r))))))))

(define (tree-member? x tree)
  (if (tree-empty? tree) #f
      (with-tree-node (tree k v l r)
        (if (= x k) #t
            (if (< x k)
                (tree-member? x l)
                (tree-member? x r))))))

(define (tree-find fn tree)
  (let loop ((x tree) (xs '()))
    (if (tree-empty? x)
        (if (null? xs) '()
            (loop (car xs) (cdr xs)))
        (if (fn (tree-key x) (tree-value x)) (car x)
            (loop (tree-left x) (cons (tree-right x) xs))))))

(define (tree-rotate-right tree)
  (if (or (tree-empty? tree) (tree-leaf? tree)) tree
      (with-tree-node (tree k v l r)
        (with-tree-node (l ck cv cl cr)
          (tree-node ck cv cl (tree-node k v cr r))))))

(define (tree-rotate-left tree)
  (if (or (tree-empty? tree) (tree-leaf? tree)) tree
      (with-tree-node (tree k v l r)
        (with-tree-node (r ck cv cl cr)
          (tree-node ck cv (tree-node k v l cl) cr)))))

(define (tree-zig-left tree)
  (tree-rotate-right tree))

(define (tree-zig-right tree)
  (tree-rotate-left tree))

(define (tree-zig-zig-left tree)
  (tree-rotate-right (tree-rotate-right tree)))

(define (tree-zig-zig-right tree)
  (tree-rotate-left (tree-rotate-left tree)))

(define (tree-zig-zag-left tree)
  (with-tree-node (tree k v l r)
    (tree-rotate-right (tree-node k v (tree-rotate-left l) r))))

(define (tree-zig-zag-right tree)
  (with-tree-node (tree k v l r)
    (tree-rotate-left (tree-node k v l (tree-rotate-right r)))))

(define (tree-splay-d x tree)
  (if (or (tree-empty? tree) (tree-leaf? tree)) tree
      (with-tree-node (tree k v l r)
        (if (= x k) tree
            (if (< x k)
                (cond
                 ((= x (tree-key l)) (tree-zig-left tree))
                 ((and (< x (tree-key l)) (= x (tree-key (tree-left l))))
                  (tree-zig-zig-left tree))
                 ((and (> x (tree-key l)) (= x (tree-key (tree-right l))))
                  (tree-zig-zag-left tree))
                 (else (tree-splay x (tree-node k v (tree-splay x l) r))))
                (cond
                 ((= x (tree-key r)) (tree-zig-right tree))
                 ((and (> x (tree-key r)) (= x (tree-key (tree-right r))))
                  (tree-zig-zig-right tree))
                 ((and (< x (tree-key r)) (= x (tree-key (tree-left r))))
                  (tree-zig-zag-right tree))
                 (else (tree-splay x (tree-node k v l (tree-splay x r))))))))))

(define (tree-splay x tree)
  (if (or (tree-empty? tree) (tree-leaf? tree)) tree
      (let loop ((tree tree))
        (with-tree-node (tree k v l r)
          (if (< x k)
              (cond
               ((= x (tree-key l)) (tree-zig-left tree))
               ((and (< x (tree-key l)) (= x (tree-key (tree-left l))))
                (tree-zig-zig-left tree))
               ((and (> x (tree-key l)) (= x (tree-key (tree-right l))))
                (tree-zig-zag-left tree))
               (else (loop (tree-node k v (loop l) r))))
              (cond
               ((= x k) tree)
               ((= x (tree-key r)) (tree-zig-right tree))
               ((and (> x (tree-key r)) (= x (tree-key (tree-right r))))
                (tree-zig-zig-right tree))
               ((and (< x (tree-key r)) (= x (tree-key (tree-left r))))
                (tree-zig-zag-right tree))
               (else (loop (tree-node k v l (loop r))))))))))

(define (tree-insert-splay x y tree)
  (tree-splay x (tree-insert x y tree)))

(define (tree-set-splay x y tree)
  (tree-splay x (tree-set x y tree)))

(define (tree-remove-splay x tree)
  (tree-remove x (tree-splay x tree)))