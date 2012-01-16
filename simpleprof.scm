(define *profile* '())
(define *samples* 0)

(define (profile-start!)
  (set! *profile* (make-table))
  (set! *samples* 0)
  (##thread-heartbeat-interval-set! (exact->inexact 1/10000))
  (##interrupt-vector-set! 1 profile-heartbeat!))

(define (profile-stop!)
  (##interrupt-vector-set! 1 ##thread-heartbeat!))

(define (identify-continuation cont)
  (let ((locat (##continuation-locat cont)))
    (if locat
        (let* ((container (##locat-container locat))
               (file (##container->path container)))
          (if file
              (let* ((filepos (##position->filepos (##locat-position locat)))
                     (line (##fixnum.+ (##filepos-line filepos) 1))
                     (col (##fixnum.+ (##filepos-col filepos) 1)))
                (list file line col))
              'unknown))
        'unknown)))

(define (profile-heartbeat!)
  (##continuation-capture
   (lambda (cont)
     (##thread-heartbeat!)
     (let ((id (identify-continuation cont)))
       (if (not (eq? id 'unknown))
           (let ((k (string-append (car id) ":" (number->string (cadr id)))))
             (set! *samples* (+ *samples* 1))
             (with-exception-catcher
              (lambda (c)
                (table-set! *profile* k 1))
              (lambda ()
                (let ((n (table-ref *profile* k)))
                  (table-set! *profile* k (+ n 1)))))))))))


(define (write-profile-report f)
  (with-output-to-file f
    (lambda ()
      (display "Profile Samples: ")
      (display *samples*)
      (newline)
      (map (lambda (x)
             (display (car x))
             (display " ")
             (display (cdr x))
             (newline))
           (sort-list cdr > (table->list *profile*))))))


