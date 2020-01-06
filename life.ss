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
(define window-width 800)
(define window-height 600)
(define block-width 8)
(define block-height 8)

(define (draw-block x y renderer intensity)
  (sdl-set-render-draw-color renderer intensity 0 0 255)
  (sdl-render-fill-rect renderer (make-temp-rect x y block-width block-height)))

(define arena-width (fx/ window-width block-width))
(define arena-height (fx/ window-height block-height))

(define (make-arena)
  (make-vector (fx* arena-width arena-height) #f))

;; cache of "arenas" to swap between for reuse
(define (make-arena-buffered)
  (let ((arenas (list (make-arena) (make-arena)))
        (index 0))
    (define (next-arena)
      (set! index (fxmodulo (fx1+ index) 2))
      (list-ref arenas index))
    next-arena))

(define get-next-arena (make-arena-buffered))

(define (arena-idx* x y)
  (let ((xx (if (fx< x 0)
              (fx+ x arena-width)
              x))
        (yy (if (fx< y 0)
              (fx+ y arena-height)
              y)))
    (fx+ (fx* (fxmodulo yy arena-height) arena-width)
         (fxmodulo xx arena-width))))

(define-syntax life-alive-p
  (syntax-rules ()
    ((_ x)
     (fx= x 255))))
(define-syntax life-drain
  (syntax-rules ()
    ((_ x)
     (cond
      ((fx> x 0) (max 0 (fx- x 16)))
      (else 0)))))
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
           (value (if (fx= 0 (modulo random-value 2))
                    0
                    255)))
      (arena-set! arena x y value))))

(define (arena-clear! arena)
  (for-arena (x y)
    (arena-set! arena x y #f)))

(define (arena-render arena renderer)
  (for-arena (x y)
    (draw-block (fx* block-width x) (fx* block-height y) renderer (arena-ref arena x y)))) 

(define (arena-display arena)
  (for (y 0 arena-height)
    (for (x 0 arena-width)
      (if (arena-ref arena x y)
        (display "X")
        (display ".")))
    (newline)))

;;;; Conway's Game of Life
;;;;
;;;; Events happen simultaneously
;;;; 1. Alive cells with fewer than 2 neighbours die.
;;;; 2. Alive cells with 2-3 neighbours lives.
;;;; 3. Alive cells with greater than 3 neighbours die.
;;;; 4. Dead cells with exacly three live neighbours comes to life.
(define (life-tick-state alive neighbours)
  (let ((alivep (life-alive-p alive)))
    (cond
     ((and alivep (< neighbours 2)) (life-drain alive))
     ((and alivep (> neighbours 3)) (life-drain alive))
     ((and (not alivep) (= neighbours 3)) 255)
     (else
      (if (life-alive-p alive)
        alive
        (life-drain alive))))))

(define (life-tick-inner arena x y)
  (let* ((alive (arena-ref arena x y))
         (neighbours (arena-surrounds-alive arena x y)))
    (life-tick-state alive neighbours)))

(define (life-tick arena)
  (let ((new-arena (get-next-arena)))
    (for-arena (x y)
      (arena-set! new-arena x y (life-tick-inner arena x y)))        
    new-arena))

(define (life-tick-pause arena)
  (let ((new-arena (get-next-arena)))
    (for-arena (x y)
      (let ((alive (arena-ref arena x y)))
        (arena-set! new-arena x y (if (life-alive-p alive)
                                    alive
                                    (life-drain alive)))))
    new-arena))

;; return number of alive neighbours for a cell
(define (arena-surrounds-alive arena x y)
  (let ((alive 0))
    (for (yy 0 3)
      (for (xx 0 3)
        (let* ((get-x (1- (fx+ x xx)))
               (get-y (1- (fx+ y yy)))
               (self (and (= get-x x) (= get-y y)))
               (alivep (life-alive-p (arena-ref arena get-x get-y))))
          (when (and alivep (not self))
            (set! alive (1+ alive))))))
    alive))

(define (arena-surrounds-display arena x y)
  (let ((surrounds (arena-surrounds arena x y)))
    (for (y 0 3)
      (for (x 0 3)
        (if (vector-ref surrounds (fx+ x (fx* y 3)))
          (display "X")
          (display ".")))
      (newline))))

(define arena (get-next-arena))
(arena-randomize! arena)

(let* ((window (sdl-create-window "Game of Life" 0 0 window-width window-height 0))
       (fullscreen #f)
       (toggle-fullscreen (lambda ()
                            (set! fullscreen (not fullscreen))
                            (sdl-set-window-fullscreen window
                                                       (if fullscreen
                                                           (sdl-window-flags 'fullscreen-desktop)
                                                           0))))
       (renderer (sdl-create-renderer window -1 (sdl-renderer-flags 'software)))
       (event (make-ftype sdl-event-t))
       (running #t)
       (current-time (sdl-get-ticks))
       (frame-limiter (make-frame-limiter 30 current-time))
       (frame-counter (make-frame-counter current-time))
       (event (make-ftype sdl-event-t))
       (frame-rate 0)
       (pause #f)
       (mtx (make-mutex))
       (life-action (lambda ()
                      (let* ((current-time (sdl-get-ticks))
                             (life-counter (make-frame-counter current-time))
                             (life-limiter (make-frame-limiter 60 current-time))
                             (last-rate 0))
                        (let loop ()
                          (let* ((current-time (sdl-get-ticks))
                                 (count (life-counter current-time))
                                 (delay-time (life-limiter current-time)))
                            (when (not (fx= last-rate count))
                              (displayln (format "life: ~a" count))
                              (set! last-rate count))
                            (sdl-delay delay-time)
                            (with-mutex mtx
                              (if pause
                                (set! arena (life-tick-pause arena))
                                (set! arena (life-tick arena)))))
                          (when running
                            (loop))))))
       (life-interval (fork-thread life-action))
       (redisplay-interval (make-interval 0 current-time
                                          (lambda ()
                                            (with-mutex mtx
                                              (arena-render arena renderer))
                                            (sdl-render-present renderer))))
       (gc-stats-interval (make-interval 5000 current-time
                                         (lambda ()
                                           (display-statistics)))))

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
      (redisplay-interval current-time)
      (gc-stats-interval current-time)

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
                              (if (life-alive-p (arena-ref arena block-x block-y))
                                0
                                255))))
               ((fx= event-type (sdl-event-type 'keydown))
                (let* ((keyboard-event (ftype-&ref sdl-event-t (key) event))
                       (keysym (ftype-&ref sdl-keyboard-event-t (keysym) keyboard-event))
                       (key-code (ftype-ref sdl-keysym-t (sym) keysym)))
                  (cond
                   ((fx= key-code (sdl-keycode 'c))
                    (with-mutex mtx
                      (arena-clear! arena)))
                   ((fx= key-code (sdl-keycode 'escape))
                    (set! running #f))
                   ((fx= key-code (sdl-keycode 'f))
                    (toggle-fullscreen))
                   ((fx= key-code (sdl-keycode 'return))
                    (with-mutex mtx
                      (arena-randomize! arena)))
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
;(exit)
