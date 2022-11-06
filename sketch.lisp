(in-package #:rht/sketch)

(defparameter *side* 50)
(defparameter *d-side* 5) ; how much space to leave between nodes
(defparameter *mover-r* 5) ; how big do we draw the mover
(defparameter *outside-weight* 5) ;how thick is border of node with goal
(defparameter *font-size* 40) ; for rotation letters
(defparameter *font-shift* 20) ; shift by y-axis to center rotate letter in the node

(defparameter *animate-time* 1/2) ; time to animate rotation

(defparameter *color-map*
  (list (cons :red +red+)
        (cons :blue +blue+)
        (cons :trash +black+)
        (cons :yellow (rgb 0.9 0.7 0))
        (cons :green (rgb 0 0.7 0))))
(defparameter *key-map*
  '((:scancode-a . :a) (:scancode-s . :s)
    (:scancode-d . :d) (:scancode-w . :w)))
(defparameter *level-map*
  '((:scancode-1 . 1) (:scancode-2 . 2) (:scancode-3 . 3) (:scancode-4 . 4)))

(defvar *font-face*)
(defparameter *running* t)

(defun draw-node (hx hy node animate? animate-progress hex-map)
  (destructuring-bind ((n angle) (x y))
      (list (node-shape hx hy) (hex-to-xy hx hy))
    (let ((origin (when animate? (loop for c in (integer-neighbours hx hy)
                                       for n = (gethash c hex-map (make-node))
                                       if (eql animate? (cadr (node-outside n)))
                                       do (return c))))
          (x (* *side* x))
          (y (* *side* y))
          (side (* (- *side* *d-side*) (ngon-scale n)))
          (sub-side (- (* (- *side* *d-side*) (ngon-scale n))
                       (* 2 *outside-weight*))))
      (do-later 1
        (with-pen (make-pen :fill +white+)
          (ngon n x y side side angle)))
      (case (car (node-outside node))
        (:color
         (do-later 3
           (with-pen (make-pen :stroke (cdr (assoc (second (node-outside node)) *color-map*))
                               :weight *outside-weight*)
             (ngon n x y sub-side sub-side angle))))
        (:bin
         (do-later 3
           (with-pen (make-pen :stroke (gray (if (cadr (node-outside node)) 0.5 0))
                               :weight *outside-weight*)
             (ngon n x y sub-side sub-side angle))))
        (:rotation
         (do-later 3
           (with-font (make-font :size *font-size* :color (case (third (node-outside node))
                                                            (:clock +blue+)
                                                            (:counter-clock +red+))
                                 :align :center :face *font-face*)
             (text (format nil "~a" (second (node-outside node)))
                   x (- y *font-shift*))))))
      (let ((r-angle 0) (r-cx 0)
            (r-cy 0) (side side))
        (when origin
          (destructuring-bind (cx cy) (apply #'hex-to-xy origin)
            (let ((angle (if (eql :clock (caddr (node-outside (gethash origin hex-map)))) 60 -60)))
              (setf r-angle (* angle animate-progress)
                    r-cx (* cx *side*)
                    r-cy (* cy *side*))
              (decf side (* *d-side* 2 (sin (* pi animate-progress)))))))
        (do-later 2
          (with-pen (make-pen :fill (gray 0 0.2))
            (with-rotate (r-angle r-cx r-cy)
              (ngon n x y side side angle))))
        (when (node-inside node)
          (do-later 3
            (with-pen (make-pen :fill (cdr (assoc (node-inside node) *color-map*)))
              (with-rotate (r-angle r-cx r-cy)
                (circle x y *mover-r*)))))))))

(defun get-animate-progress (animate-start animate?)
  (when animate?
    (/ (- (get-internal-real-time) animate-start) internal-time-units-per-second
       *animate-time*)))

(defun draw-hexagon-map (hexagon-map animate? animate-progress)
  (maphash (lambda (c node) (draw-node (car c) (cadr c) node animate? animate-progress hexagon-map))
           hexagon-map)
  (do-now))

(defsketch draw-level ((level-number 1)
                       (level (level 1))
                       (animate? nil)
                       (animate-start 0)
                       (width 800)
                       (height 800))
  (background (hex-to-color "ab5130"))
  (let ((animate-progress (get-animate-progress animate-start animate?)))
    (when (and animate? (> animate-progress 0.95))
      (level-step level animate?)
      (setf animate? nil))
    (destructuring-bind (x y w h) (bounds (level-hexagon-map level))
      (with-fit ((* *side* w) (* *side* h) 400 400
                 (* *side* x) (* *side* y) 200 200 1)
        (draw-hexagon-map (level-hexagon-map level) animate? animate-progress))))
  (with-font (make-font :align :left :size 50 :face *font-face*)
    (text (format nil "level: ~a" level-number) 300 0)
    (text (format nil "steps: ~a ~{/ ~a~}" (level-steps level)
                  (unless (zerop (level-max-steps level)) (list (level-max-steps level))))
          300 50))
  (with-font (make-font :color (gray .8) :size 50 :face *font-face*)
    (text (case (level-state level)
            (:won "Hurray! :)")
            (:play "Hm... What do I do?")
            (:lost "x-x"))
          100 100)))
#+deploy
(progn (defun sketch::make-default-font ())
       (defun sketch::make-error-font ()))

(defmethod setup ((app draw-level) &key &allow-other-keys)
  (setf *font-face* (load-resource (data-path "RobotoMono-ExtraLight.ttf"))))

(defmethod kit.sdl2:close-window :before ((app draw-level))
  (setf *running* nil))

(defmethod kit.sdl2:keyboard-event ((app draw-level) state ts repeat? keysym
                                    &aux (key (sdl2:scancode keysym)))
  (when (and (eq state :keydown))
    (with-slots (level level-number appdata animate? animate-start) app
      (cond
        (animate?)
        ((eql key :scancode-r) (setf level (level level-number)))
        ((eql key :scancode-n) (when (< level-number *levels*)
                                 (incf level-number)
                                 (setf level (level level-number))))
        ((eql key :scancode-p) (when (> level-number 1)
                                 (decf level-number)
                                 (setf level (level level-number))))
        ((assoc key *level-map*) (setf level-number (cdr (assoc key *level-map*))
                                       level (level level-number)))
        ((not (eql :play (level-state level))))
        ((assoc key *key-map*)
         (setf animate? (cdr (assoc key *key-map*)))
         (setf animate-start (get-internal-real-time)))))))

(defun start ()
  (make-instance 'draw-level)
  (loop while *running*
        do (sleep 1)))
