#!/usr/bin/env scheme
;;;; Game of Life
;;;;
;;;; Notes:
;;;; * Requires Chez Scheme 9.5.3+ for foreign pass-by-struct.
;;;; * Uses modified Thunderchez SDL2 FFI bindings to use pass-by-struct
;;;;   rather than similarly sized integer types. Doing so removes the
;;;;   need for any pointer-deref shims.

(library-directories "~/thunderchez")

(import (sdl2))
(import (sdl2 image))
(import (chezscheme))
(import (srfi s48 intermediate-format-strings))

(sdl-library-init)
(sdl-image-library-init)

;;;; helpers
(define (make-interval interval init-time action)
  (let ((last-time init-time))
    (lambda (current-time)
      (when (fx> current-time (fx+ last-time interval))
        (action)
        (set! last-time (fx+ last-time interval))))))

(define displayln
  (case-lambda
    ((str port)
     (display str port)(newline port))
    ((str)
     (display str)(newline))))

(define-syntax for
  (syntax-rules ()
    ((for (var init end) stmt ...)
     (let ((var init))
       (let loop ()
         (when (< var end)
           (begin
             stmt ...)
           (set! var (1+ var))
           (loop)))))))

;; TODO fix precision of limiting
(define (make-frame-limiter frame-max initial-time)
  (let ((last-time initial-time)
        (max-delay (fx/ 1000 frame-max)))
    (lambda (current-time)
      (let* ((since (fx- current-time last-time))
             (delay-time (if (fx<= since 1)
                           0
                           (fx/ 1000 since))))
        (set! last-time current-time)
        (min delay-time max-delay)))))

(define (make-frame-counter initial-time)
  (let ((next-time (fx+ initial-time 1000))
        (fps 0)
        (counter 0))
    (lambda (current-time)
      (set! counter (fx1+ counter))
      (when (fx>= current-time next-time)
        (set! fps counter)
        (set! counter 0)
        (set! next-time (fx+ next-time 1000)))
      fps)))

(define make-ftype-guardian (make-guardian))
(define (make-ftype-guardian-collect)
  (let loop ()
    (let ([x (make-ftype-guardian)])
      (when x
        (display "freeing ")
        (display x)(newline)
        (foreign-free (ftype-pointer-address x))
        (loop)))))

(define-syntax make-ftype
  (syntax-rules ()
    ((_ type)
     (let ((ptr (make-ftype-pointer type
                                    (foreign-alloc (ftype-sizeof type)))))
       (make-ftype-guardian-collect)
       (make-ftype-guardian ptr)
       ptr))))

(define-syntax literal-color
  (syntax-rules ()
    ((_ red green blue)
     (let ([fptr (make-ftype sdl-color-t)])
       (ftype-set! sdl-color-t (r) fptr red)
       (ftype-set! sdl-color-t (g) fptr green)
       (ftype-set! sdl-color-t (b) fptr blue)
       fptr))))

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

;; FIXME thread-safety
(define make-temp-rect
  (let ([fptr (make-rect 0 0 0 0)])
    (lambda (x y w h)
      (ftype-set! sdl-rect-t (x) fptr x)
      (ftype-set! sdl-rect-t (y) fptr y)
      (ftype-set! sdl-rect-t (w) fptr w)
      (ftype-set! sdl-rect-t (h) fptr h)
      fptr)))

(define (make-temp-surface-rect surface)
  (make-temp-rect 0
                  0
                  (1- (ftype-ref sdl-surface-t (w) surface))
                  (1- (ftype-ref sdl-surface-t (h) surface))))

(define (get-surface-rect surface)
  (make-rect 0
             0
             (1- (ftype-ref sdl-surface-t (w) surface))
             (1- (ftype-ref sdl-surface-t (h) surface))))

;;;; setup
(define window-width 1024)
(define window-height 768)
(define block-width 24)
(define block-height 24)

(define (draw-block x y surface)
  (sdl-fill-rect surface (make-temp-rect x y block-width block-height) (sdl-map-rgb (ftype-ref sdl-surface-t (format) surface) 255 0 0)))
  
(define (draw-empty x y surface)
  (sdl-fill-rect surface (make-temp-rect x y block-width block-height) (sdl-map-rgb (ftype-ref sdl-surface-t (format) surface) 0 0 0)))

(define arena-width (fx/ window-width block-width))
(define arena-height (fx/ window-height block-height))

(define (make-arena)
  (make-vector (fx* arena-width arena-height) #f))

(define (arena-idx* x y)
  (let ((xx (if (fx< x 0)
              (fx+ x arena-width)
              x))
        (yy (if (fx< y 0)
              (fx+ y arena-height)
              y)))
    (fx+ (fx* (fxmodulo yy arena-height) arena-width)
         (fxmodulo xx arena-width))))

(define-syntax for-arena
  (syntax-rules ()
    ((for-arena (x y) stmt stmts ...)
     (for (y 0 arena-height)
       (for (x 0 arena-width)
         stmt
         stmts ...)))))

(define (arena-ref arena x y)
  (vector-ref arena (arena-idx* x y)))

(define (arena-set! arena x y v)
  (vector-set! arena (arena-idx* x y) v))

(define (arena-randomize! arena)
  (for-arena (x y) 
    (let* ((random-value (random 2))
           (value (= 0 (modulo random-value 2))))
      (arena-set! arena x y value))))

(define (arena-clear! arena)
  (for-arena (x y)
    (arena-set! arena x y #f)))

(define (arena-render arena surface)
  (for-arena (x y)
    (if (arena-ref arena x y)
      (draw-block (fx* block-width x) (fx* block-height y) surface)
      (draw-empty (fx* block-width x) (fx* block-height y) surface))))

(define (arena-display arena)
  (for (y 0 arena-height)
    (for (x 0 arena-width)
      (if (arena-ref arena x y)
        (display "X")
        (display ".")))
    (newline)))

(define arena (make-arena))

;;;; Conway's Game of Life
;;;;
;;;; Events happen simultaneously
;;;; 1. Alive cells with fewer than 2 neighbours die.
;;;; 2. Alive cells with 2-3 neighbours lives.
;;;; 3. Alive cells with greater than 3 neighbours die.
;;;; 4. Dead cells with exacly three live neighbours comes to life.
(define (life-tick-state alive neighbours)
  (cond
   ((and alive (< neighbours 2)) #f)
   ((and alive (> neighbours 3)) #f)
   ((and (not alive) (= neighbours 3)) #t)
   (else alive)))

(define (vector-fold fn init vec)
  (define (vector-iterate vec fn)
    (let ([len (vector-length vec)])
      (define (do idx)
        (when (< idx len)
          (fn idx (vector-ref vec idx))
          (do (1+ idx))))
      (do 0)))
  (let ([result init])
    (vector-iterate vec
                    (lambda (idx v)
                      (set! result (fn idx result v))))
    result))

(define (life-tick-inner arena x y)
  (let* ((alive (arena-ref arena x y))
         (surrounds (arena-surrounds arena x y))
         (neighbours (fx- (vector-fold (lambda (idx acc val)
                                         (if val
                                           (1+ acc)
                                           acc))
                                       0
                                       surrounds)
                          (if alive 1 0))))
    (life-tick-state alive neighbours)))

(define (life-tick arena)
  (let ((new-arena (make-arena)))
    (for-arena (x y)
      (arena-set! new-arena x y (life-tick-inner arena x y)))        
    new-arena))

;; return a 3x3 vector of surrounds
(define (arena-surrounds arena x y)
  (define (doit result)
    (for (yy 0 3)
      (for (xx 0 3)
        (let ((get-x (1- (fx+ x xx)))
              (get-y (1- (fx+ y yy))))
            (vector-set! result (fx+ xx (fx* yy 3))
                         (arena-ref arena get-x get-y)))))
    result)
  (let ((result (make-vector (fx* 3 3) #f)))
    (doit result)))

(define (arena-surrounds-display arena x y)
  (let ((surrounds (arena-surrounds arena x y)))
    (for (y 0 3)
      (for (x 0 3)
        (if (vector-ref surrounds (fx+ x (fx* y 3)))
          (display "X")
          (display ".")))
      (newline))))

(arena-randomize! arena)

(let* ((window (sdl-create-window "Game of Life" 0 0 window-width window-height 0))
       (surface (sdl-get-window-surface window))
       (event (make-ftype sdl-event-t))
       (running #t)
       (surface-rect (get-surface-rect surface))
       (current-time (sdl-get-ticks))
       (frame-limiter (make-frame-limiter 60 current-time))
       (frame-counter (make-frame-counter current-time))
       (event (make-ftype sdl-event-t))
       (frame-rate 0)
       (pause #f)
       (life-interval (make-interval 100 current-time
                                          (lambda ()
                                            (unless pause
                                              (set! arena (life-tick arena))))))
       (redisplay-interval (make-interval 100 current-time
                                          (lambda ()
                                            (arena-render arena surface)
                                            (sdl-update-window-surface window)))))

  (let loop ()
    (let* ([current-time (sdl-get-ticks)]
           [delay-time (frame-limiter current-time)])
      (sdl-delay delay-time)

      ;; only display FPS on rate change
      (let ((new-frame-rate (frame-counter current-time)))
        (when (not (fx= frame-rate new-frame-rate))
          (displayln (format "fps: ~a" new-frame-rate))
          (set! frame-rate new-frame-rate)))

      ;; run simulation and redraw
      (life-interval current-time)
      (redisplay-interval current-time)

      (let event-loop ()
        (let ((e (sdl-poll-event event)))
          (when (> e 0)
            (let ((event-type (ftype-ref sdl-event-t (type) event)))
              (cond
               ((fx= event-type (sdl-event-type 'quit))
                (set! running #f))
               ((fx= event-type (sdl-event-type 'mousebuttondown))
                (let* ((button-event (ftype-&ref sdl-event-t (button) event))
                       (button (ftype-ref sdl-mouse-button-event-t (button) button-event))
                       (x (ftype-ref sdl-mouse-button-event-t (x) button-event))
                       (y (ftype-ref sdl-mouse-button-event-t (y) button-event))
                       (block-x (fx/ x block-width))
                       (block-y (fx/ y block-height)))
                  (arena-set! arena block-x block-y
                              (not (arena-ref arena block-x block-y)))))
               ((fx= event-type (sdl-event-type 'keydown))
                (let* ((keyboard-event (ftype-&ref sdl-event-t (key) event))
                       (keysym (ftype-&ref sdl-keyboard-event-t (keysym) keyboard-event))
                       (key-code (ftype-ref sdl-keysym-t (sym) keysym)))
                  (cond
                   ((fx= key-code (sdl-keycode 'c))
                    (arena-clear! arena))
                   ((fx= key-code (sdl-keycode 'escape))
                    (set! running #f))
                   ((fx= key-code (sdl-keycode 'return))
                    (arena-randomize! arena))
                   ((fx= key-code (sdl-keycode 'space))
                    (set! pause (not pause))
                    (if pause
                      (displayln "Paused. Press space to unpause.")
                      (displayln "Unpaused. Press space to pause.")))
                   (else
                    (displayln (format "Unhandled keycode: ~a"
                                       (if (< key-code 256)
                                         (begin
                                           (let ((key-char (integer->char key-code)))
                                             (if (or (char-alphabetic? key-char)
                                                     (char-numeric? key-char))
                                               key-char
                                               key-code)))
                                         key-code)))))))))
            (event-loop))))

      (when running (loop))))

  (sdl-destroy-window window))

;; (sdl-quit)

(displayln "Goodbye :(")
(exit)
