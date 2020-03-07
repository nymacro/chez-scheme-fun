;;;; taken from On Lisp with modifications
(define *paths* '())
(define *no-match* 'fail)

(define (reset)
  (set! *paths* '()))

(define fail)

(define (choose choices)
  (if (null? choices)
    (fail)
    (call/cc
      (lambda (cc)
        (set! *paths*
          (cons (lambda ()
                  (cc (choose (cdr choices))))
                *paths*))
        (car choices)))))

(define-syntax choosex
  (syntax-rules ()
    ((_ choices ...)
     (choose '(choices ...)))))

(call/cc
  (lambda (cc)
    (set! fail
      (lambda ()
        (if (null? *paths*)
          (cc *no-match*)
          (let ((p1 (car *paths*)))
            (set! *paths* (cdr *paths*))
            (p1)))))))

(define (xx x y)
  (if (and (odd? x) (= 5 x)
           (= (+ x y) 11))
    (list x y)
    (fail)))

(display (xx (choosex 1 2 3 4 5 6 7) (choosex 1 2 4 5 6 7)))
(newline)
