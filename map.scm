(define (make-map)
  (make-tree))

(define (map-set x y map)
  (tree-set-splay x y map))

(define (map-ref x map)
  (tree-ref x map))

(define (map-remove x map)
  (tree-remove-splay x map))
