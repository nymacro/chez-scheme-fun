#!/usr/bin/env chez-scheme
;;
;; ChezScheme SDL FFI test
;;
;; TODO:
;; * proper cleanup (GC tie-in etc.)

(library-directories "~/thunderchez")
(define ttf-font-file "/usr/local/share/fonts/hack-font/Hack-Bold.ttf")

(define displayln
  (case-lambda
    ((str port)
     (display str port)(newline))
    ((str)
     (display str)(newline))))

(import (sdl2))
(import (sdl2 ttf))
(import (chezscheme))

(begin
  (define has-init #f)
  (when (not has-init)
    (sdl-library-init)
    (sdl-ttf-library-init)
    (set! has-init #t)))

(ttf-init)


(define (make-frame-limiter frame-max initial-time)
  (let ((last-time initial-time)
        (max-delay (fx/ 1000 frame-max)))
    (lambda (current-time)
      (let ((since (fx- current-time last-time)))
        (set! last-time current-time)
        (min (fx/ 1000 (max 1 since))
             max-delay)))))

(define (make-frame-counter initial-time)
  (let ((last-time initial-time)
        (fps 0)
        (counter 0))
    (lambda (current-time)
      (set! counter (fx1+ counter))
      (when (fx> current-time (fx+ last-time 1000))
        (set! fps counter)
        (set! counter 0)
        (set! last-time current-time))
      fps)))

(define (make-circular fn distance span-time init-time)
  (let* ((last-time init-time)
         (angle 0)
         (pi 3.14159)
         (fix (lambda (i)
                (flonum->fixnum (exact->inexact i)))))
    (lambda (current-time)
      (let* ((elapsed (fx- current-time last-time))
             (ratio (/ elapsed span-time))
             (new-angle (* 2 pi ratio))
             (nx (* (fx/ distance 2) (fn new-angle))))
        (when (> new-angle (* 2 pi))
          (set! last-time current-time))
        (fix nx)))))

(define (make-sine distance span-time init-time)
  (make-circular sin distance span-time init-time))

(define (make-cosine distance span-time init-time)
  (make-circular cos distance span-time init-time))

(define (make-orbit distance span-time init-time)
  (let ((s (make-sine distance span-time init-time))
        (c (make-cosine distance span-time init-time)))
    (lambda (current-time)
      (cons (c current-time) (s current-time)))))

(define-syntax make-ftype
  (syntax-rules ()
    ((_ type)
     (make-ftype-pointer type
                         (foreign-alloc (ftype-sizeof type))))))

(define-syntax literal-color
  (syntax-rules ()
    ((_ red green blue)
     (let ([fptr (make-ftype sdl-color-t)])
       (ftype-set! sdl-color-t (r) fptr red)
       (ftype-set! sdl-color-t (g) fptr green)
       (ftype-set! sdl-color-t (b) fptr blue)
       ;; this is a fragile way of passing SDL_Color by value --
       ;; is there a better way of doing this???
       (foreign-ref 'unsigned-32 (ftype-pointer-address fptr) 0)))))

(define color-white (literal-color 255 255 255))
(define color-cyan (literal-color 255 0 255))
(define color-black (literal-color 0 0 0))

(define-syntax make-rect
  (syntax-rules ()
    ((_ x* y* w* h*)
     (let ([fptr (make-ftype sdl-rect-t)])
       (ftype-set! sdl-rect-t (x) fptr x*)
       (ftype-set! sdl-rect-t (y) fptr y*)
       (ftype-set! sdl-rect-t (w) fptr w*)
       (ftype-set! sdl-rect-t (h) fptr h*)
       fptr))))

(define ttf-font (ttf-open-font ttf-font-file 48))
(define hello-surface (ttf-render-text-blended ttf-font "Hello!" color-cyan))
(define empty-rect (make-rect 0 0 0 0))

(define (get-surface-rect surface)
  (make-rect 0
             0
             (1- (ftype-ref sdl-surface-t (w) surface))
             (1- (ftype-ref sdl-surface-t (h) surface))))

(let* ((window (sdl-create-window "Hello SDL" 0 0 800 600 0))
       (surface (sdl-get-window-surface window))
       (event (make-ftype sdl-event-t))
       (running #t)
       (surface-rect (get-surface-rect surface))
       (current-time (sdl-get-ticks))
       (orbit (make-orbit 200 2000 current-time))
       (waver (make-sine 400 1250 current-time))
       (frame-limiter (make-frame-limiter 60 current-time))
       (frame-counter (make-frame-counter current-time))
       (frame-rate 0))

  (let loop ()
    (let* ((current-time (sdl-get-ticks))
           (delay-time (frame-limiter current-time))) 
      (sdl-delay delay-time)

      ;; only display FPS on rate change
      (let ((new-frame-rate (frame-counter current-time)))
        (when (not (fx= frame-rate new-frame-rate))
          (displayln (format "fps: ~a" (frame-counter current-time))))
        (set! frame-rate new-frame-rate))

      (let event-loop ()
        (let ((e (sdl-poll-event event))
              (displayln (lambda x #f)))
          (when (> e 0)
            (let ((event-type (ftype-ref sdl-event-t (type) event)))
              (cond
               ((fx= event-type (sdl-event-type 'quit))
                (set! running #f))
               ((fx= event-type (sdl-event-type 'mousemotion))
                (displayln "mouse motion"))
               ((fx= event-type (sdl-event-type 'keydown))
                (let* ((keyboard-event (ftype-&ref sdl-event-t (key) event))
                       (keysym (ftype-&ref sdl-keyboard-event-t (keysym) keyboard-event))
                       (key-code (ftype-ref sdl-keysym-t (sym) keysym)))
                  (cond
                   ((fx= key-code (sdl-keycode 'escape))
                    (set! running #f)))))))
            (event-loop))))

      ;; redisplay
      (sdl-fill-rect surface
                     surface-rect
                     (sdl-map-rgb (ftype-ref sdl-surface-t (format) surface) 0 0 0))

      (let ((pair (orbit current-time))
            (x-offset (fx- (fx/ (ftype-ref sdl-surface-t (w) surface) 2)
                           (fx/ (ftype-ref sdl-surface-t (w) hello-surface) 2)))
            (y-offset (fx- (fx/ (ftype-ref sdl-surface-t (h) surface) 2)
                           (fx/ (ftype-ref sdl-surface-t (h) hello-surface) 2))))
        (sdl-upper-blit hello-surface (get-surface-rect hello-surface)
                        surface (make-rect (fx+ x-offset
                                                (waver current-time)
                                                (car pair))
                                           (fx+ y-offset
                                                (cdr pair))
                                           0 0)))

      ;; flip surface
      (sdl-update-window-surface window)

      (when running (loop))))

  (sdl-destroy-window window))

;; (ttf-quit)
;; (sdl-quit)

(displayln "Goodbye :(")
