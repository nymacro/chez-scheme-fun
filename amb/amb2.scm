;;;; Reference implementation from https://ebzzry.io/en/amb/
;; (define f #f)                                   ;  3
;;                                                 ;  4
;; (define-syntax amb                              ;  5
;;   (syntax-rules ()                              ;  6
;;     ((_) (f))                                   ;  7
;;     ((_ a) a)                                   ;  8
;;     ((_ a b ...)                                ;  9
;;      (let ((s f))                               ; 10
;;        (call/cc                                 ; 11
;;         (lambda (k)                             ; 12
;;           (set! f (lambda ()                    ; 13
;;                     (set! f s)                  ; 14
;;                     (k (amb b ...))))           ; 15
;;           (k a)))))))                           ; 16
;;                                                 ; 17
;; (define (really? x y)                           ; 18
;;   (if (equal? x y)                              ; 19
;;       (list x y)                                ; 20
;;       (amb)))                                   ; 21
;;                                                 ; 22
;; (call/cc                                        ; 23
;;  (lambda (k)                                    ; 24
;;    (set! f (lambda ()                           ; 25
;;              (k 'no-choices)))))                ; 26


;;;; from-scratch impl which doesn't support multiple choices...
;; (define fail)
;; (define (amb . choices)
;;   (call/cc
;;     (lambda (cc)
;;       (if (null? choices)
;;         (fail))
;;       (set! fail
;;         (lambda ()
;;           (set! choices (cdr choices))
;;           (if (null? choices)
;;             (error "exhausted"))
;;           (cc (car choices))))
;;       (car choices))))

;;;; This implementation accumulates continuation state
;;;; when paths are not exhausted.
(define amb-fail '())
(define (amb . choices)
  (call/cc
    (lambda (cc)
      (cond
       ((null? choices)
        (if (null? amb-fail)
          (error amb "exhausted"))
        ((car amb-fail)))
       (else
        (set! amb-fail
          (cons (lambda ()
                  (set! choices (cdr choices))
                  (cond
                   ((null? choices)
                    (set! amb-fail (cdr amb-fail))
                    ((car amb-fail)))
                   (else
                    (cc (car choices)))))
                amb-fail))
        (car choices))))))


;;;; Another implementation that accumulates continuation state
;;;; when paths are not exhausted...
;;;;
;;;; Iterates over all choices and build up a set
;;;; of continuations which will return a value
;;;; AND set amb-fail back to its previous value.
;;;;
;;;; This approach still leaves around unexhausted choices in
;;;; amb-fail -- whose accumulation causes issues for memory
;;;; and correctness.
;;;;
;;;; The scope of amb-fail is dynamic, and therefore
;;;; can return values even out of the scope of functions
;;;; which it was used.
(define amb-fail #f)
(define (amb . choices)
  (call/cc
    (lambda (cc)
      (cond
       ((null? choices)
        (amb-fail))
       (else
        (let loop ()
          (let ((old-fail amb-fail)
                (choice (car choices)))
            (set! choices (cdr choices))
            (set! amb-fail
              (lambda ()
                (set! amb-fail old-fail)
                (cc choice)))
            (unless (null? choices)
              (loop))))
        (amb-fail))))))

(set! amb-fail (lambda () 'no-choice))



(define-syntax all-matches
  (syntax-rules ()
    ((_ expr ...)
     (let ((matches '()))
       (let ((m (begin
                  expr ...)))
         (when (not (equal? m 'no-choice))
           (set! matches (cons m matches))
           (amb))
         matches)))))

(define (find-stuff)
  (define a (amb 1 2 3))
  (define b (amb 1 2 3))
  (if (= (* a b) 9)
    (list a b)
    (amb)))

(define (find-stuff2)
  (define a (amb 1 2 3))
  (define b (amb 1 2 3))
  (define c (amb 1 2 3))
  ;; consumes all continuations when all possibilities are exhausted
  (if (= (+ c (* a b)) 9)
    (list a b c)
    (amb)))

(define (leak-amb)
  (let ((called 0))
    (define (test)
      (define a (amb 1 2 3))
      (define b (amb 1 2 3))
      (set! called (1+ called))
      (if (= a b)
        (display (format "~a~%" (list a b)))
        (amb)))
    (display (format "This should only return once, and then return no-choice~%"))
    (test)
    (amb)
    (display (format "expect 1, got: ~a~%" called))
    (= called 1)))

(define (test-mem)
  (let loop ((i 1000000))
    (find-stuff2)
    (when (> i 0)
      (loop (1- i)))))
