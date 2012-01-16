
(define (findpath-reconstruct-path start end path-map)
  (let loop ((node end)
             (acc '()))
    (if (= start node)
        (cons node acc)
        (loop (map-ref node path-map) (cons node acc)))))

(define (findpath start goal dist heur get-negibours)
  (let outer ((x start)
              (open (make-pqueue))
              (seen (make-set))
              (closed (make-set))
              (cost-map (map-set start 0 (make-map)))
              (path-map (make-map)))
    (if (goal x)
        (findpath-reconstruct-path start x path-map)
        (let inner ((ys (get-negibours x))
                    (open open)
                    (seen seen)
                    (closed (set-add x closed))
                    (cost-map cost-map)
                    (path-map path-map))
          (if (null? ys)
              (if (pqueue-empty? open) '()
                  (call-with-values (lambda () (pqueue-out open))
                    (lambda (x open)
                      (outer x open seen closed cost-map path-map))))
              (let* ((y (car ys))
                     (cost (+ (map-ref x cost-map)
                              (dist x y)
                              (heur y))))
                (cond
                 ((set-member? y closed)
                  (inner (cdr ys) open seen closed cost-map path-map))
                 ((not (set-member? y seen))
                  (inner (cdr ys) (pqueue-in cost y open)
                         (set-add y seen)
                         closed
                         (map-set y cost cost-map)
                         (map-set y x path-map)))
                 ((< cost (map-ref y cost-map))
                  (inner (cdr ys)
                         open
                         seen
                         closed
                         (map-set y cost cost-map)
                         (map-set y x path-map)))
                 (else
                  (inner (cdr ys) open seen closed cost-map path-map)))))))))


