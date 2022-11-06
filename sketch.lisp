(in-package #:rht/sketch)

(defparameter *side* 50)
(defparameter *d-side* 5) ; how much space to leave between nodes
(defparameter *mover-r* 5) ; how big do we draw the mover
(defparameter *outside-weight* 5) ;how thick is border of node with goal
(defparameter *font-size* 35) ; for rotation letters
(defparameter *level-text-size* 40) ; for level: steps: e.t.c
(defparameter *tutorial-text-size* 35) ; for special texts
(defparameter *font-shift* 23) ; shift by y-axis to center rotate letter in the node
(defparameter *font-name* "RobotoMono-Bold.ttf")

(defparameter *animate-time* 1/2) ; time to animate rotation

(defparameter *color-map*
  (list (cons :red                     (hex-to-color "c92b44"))
        (cons :blue                    (hex-to-color "00a6ba"))
        (cons :yellow                  (hex-to-color "00a6ba"))
        (cons :green                   (hex-to-color "007e4b"))
        (cons :trash                   (hex-to-color "000000"))
        (cons :unsafe-bin              (hex-to-color "000000"))
        (cons :safe-bin                (hex-to-color "ffffff"))
        (cons :background-node         (hex-to-color "007e4b"))
        (cons :foreground-node         (hex-to-color "ffef7d"))
        (cons :foreground-arrived-node (hex-to-color "c4d552"))
        (cons :->>                     (hex-to-color "90352f"))
        (cons :->o                     (hex-to-color "90352f"))
        (cons :rotation-text           (hex-to-color "401a22"))
        (cons :level-text              (hex-to-color "ffb632"))
        (cons :background              (hex-to-color "90352f"))))
(defun color (color-name)
  (cdr (assoc color-name *color-map*)))

(defparameter *key-map*
  '((:scancode-a . :a) (:scancode-s . :s)
    (:scancode-d . :d) (:scancode-w . :w)))
(defparameter *level-map*
  '((:scancode-1 . 1) (:scancode-2 . 2) (:scancode-3 . 3) (:scancode-4 . 4)))

(defparameter *font-face* nil)
(defparameter *running* t)
(defparameter *centered* nil)

(defun draw--> (direction side)
  (with-current-matrix
    (unless (eql direction :clock)
      (scale -1 1))
    (loop for i from -1 to 1
          with x = (* side 1/4)
          with y = (* side -1/2)
          with r = (/ side 12)
          do (with-rotate ((* 30 i))
               (if (= i 1)
                   (with-pen (make-pen :fill (color :->>))
                     (ngon 3 x y (* r 2) (* r 2) 10))
                   (with-pen (make-pen :fill (color :->o))
                     (circle x y r)))))))

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
                       (* 2 *outside-weight* (if (= n 4) (ngon-scale n) 1)))))
      (do-later 1
        (with-pen (make-pen :fill (color :background-node))
          (ngon n x y side side angle)))
      (let ((r-angle 0) (r-cx 0)
            (r-cy 0) (r-side side))
        (when origin
          (destructuring-bind (cx cy) (apply #'hex-to-xy origin)
            (let ((angle (if (eql :clock (caddr (node-outside (gethash origin hex-map)))) 60 -60)))
              (setf r-angle (* angle animate-progress)
                    r-cx (* cx *side*)
                    r-cy (* cy *side*))
              (decf r-side (* *d-side* 2 (sin (* pi animate-progress)))))))
        (do-later 2
          (with-pen (make-pen :fill (color (if (and (or (not origin) (= n 6))
                                                    (arrived? node))
                                               :foreground-arrived-node
                                               :foreground-node)))
            (with-rotate (r-angle r-cx r-cy)
              (ngon n x y r-side r-side angle))))
        (when (node-inside node)
          (do-later 3
            (with-pen (make-pen :fill (color (node-inside node)))
              (with-rotate (r-angle r-cx r-cy)
                (circle x y *mover-r*)))))
        (case (car (node-outside node))
          (:color
           (do-later 3
             (with-pen (make-pen :stroke (color (second (node-outside node)))
                                 :weight *outside-weight*)
               (ngon n x y sub-side sub-side angle))))
          (:bin
           (do-later 3
             (with-pen (make-pen :stroke (color (if (cadr (node-outside node)) :safe-bin :unsafe-bin))
                                 :weight *outside-weight*)
               (ngon n x y sub-side sub-side angle)))
           (when (node-inside node)
             (do-later 4
               (with-pen (make-pen :fill (apply-alpha (color :unsafe-bin) 0.3))
                 (circle x y (* 3 *mover-r*))))))
          (:rotation
           (let ((angle (* (if (eql :clock (third (node-outside node))) 60 -60)
                           (fourth (node-outside node)))))
             (do-later 3
               (with-translate (x y)
                 (with-rotate ((+ angle r-angle))
                   (draw--> (third (node-outside node)) side)
                   (with-rotate (180)
                     (draw--> (third (node-outside node)) side))))
               (with-font (make-font :size *font-size* :align :center
                                     :face *font-face* :color (color :rotation-text))
                 (text (format nil "~a" (second (node-outside node)))
                       x (- y *font-shift*)))))))))))

(defun get-animate-progress (animate-start animate?)
  (when animate?
    (/ (- (get-internal-real-time) animate-start) internal-time-units-per-second
       *animate-time*)))

(defun draw-hexagon-map (hexagon-map animate? animate-progress)
  (maphash (lambda (c node) (draw-node (car c) (cadr c) node animate? animate-progress hexagon-map))
           hexagon-map)
  (do-now))

(defun draw-level (level-number level animate? animate-progress)
  (destructuring-bind (x y w h) (bounds (level-hexagon-map level))
    (with-fit ((* *side* w) (* *side* h) 800 500
               (* *side* x) (* *side* y) 0   200 3/2)
      (draw-hexagon-map (level-hexagon-map level) animate? animate-progress)))
  (with-pen (make-pen :stroke (color :foreground-node)
                      :fill (apply-alpha (color :background-node) 0.1))
    (rect 0 100 800 5)
    (rect 0 700 800 5))
  (with-font (make-font :size *level-text-size* :face *font-face* :color (color :level-text))
    (text (format nil "LEVEL: ~a~%STEPS: ~a~{ / ~a~}" level-number (level-steps level)
                  (unless (zerop (level-max-steps level)) (list (level-max-steps level))))
          400 -5))
  (with-font (make-font :color (color :level-text) :size *level-text-size*
                        :face *font-face* :align :center :line-height 1.1)
    (text (case (level-state level) (:won "Solved!") (:play "...") (:lost "Fail :("))
          200 25))
  (with-font (make-font :color (color :level-text) :size *tutorial-text-size*
                        :face *font-face* :align :center :line-height 1.1)
   (text (case (level-state level)
           (:won "[press N for next level]")
           (:lost "[press R to restart]")
           (:play (format nil (level-text level))))
         400 125))
  (with-font (make-font :size (/ *level-text-size* 2) :face *font-face* :color (color :level-text)
                        :align :center)
    (text (format nil "Press M to go to menu~%[your progress will be saved]") 400 720)))

(defun draw-mute-hexagon (muted? animate? animate-progress)
  (with-pen (make-pen :fill (color :background-node))
    (ngon 6 0 0 *side* *side*))
  (with-current-matrix
    (when (eql animate? :mute)
      (rotate (* (if muted? -180 180) animate-progress))
      (let ((s (- 1 (* 1/4 (sin (* pi animate-progress))))))
        (scale s s 0 0)))
    (if muted? (rotate 180))
    (with-pen (make-pen :fill (color :foreground-node))
      (ngon 6 0 0 *side* *side*))
    (with-pen (make-pen :stroke (color :rotation-text) :weight 5)
      (line 0 0 *side* 0))))

(defstruct menu
  (level 1)
  (muted? nil))

(defun draw-menu (menu animate? animate-progress)
  (with-pen (make-pen :stroke (color :foreground-node)
                      :fill (apply-alpha (color :background-node) 0.1))
    (rect 0 100 800 5)
    (rect 0 700 800 5))
  (with-font (make-font :color (color :level-text) :size *level-text-size*
                        :face *font-face* :align :center :line-height 1.1)
    (text "Press SPACE to start" 400 25))
  (with-font (make-font :color (color :level-text) :size (/ *level-text-size* 2)
                        :face *font-face* :align :center :line-height 1.1)
    (text "Press M to (un)mute sound:" 200 740))
  (with-translate (600 750)
    (let ((*side* 30))
      (draw-mute-hexagon (menu-muted? menu) animate? animate-progress)
      (with-font (make-font :color (color :level-text) :size (/ *level-text-size* 2)
                            :face *font-face* :align :left)
        (text "PLAYING" (+ 10 *side*) -15))
      (with-font (make-font :color (color :level-text) :size (/ *level-text-size* 2)
                            :face *font-face* :align :right)
        (text "MUTED" (- -10 *side*) -15)))))

(defun menu-step (menu animate?)
  (case animate?
    (:level+ (incf (menu-level menu)))
    (:level- (decf (menu-level menu)))
    (:mute (setf (menu-muted? menu)
                 (not (menu-muted? menu))))))

(defsketch rht-game ((level-number 1)
                     (level (level 1))
                     (animate? nil)
                     (animate-start 0)
                     (buffer :menu)
                     (menu (make-menu))
                     (width 800)
                     (height 800))
  (unless *centered*
    (center-sketch sketch::instance)
    (setf *centered* t))
  (background (color :background))
  (case buffer
    (:menu
     (let ((animate-progress (get-animate-progress animate-start animate?)))
       (when (and animate? (> animate-progress 0.97))
         (menu-step menu animate?)
         (setf animate? nil))
       (draw-menu menu animate? animate-progress)))
    (:level
     (let ((animate-progress (get-animate-progress animate-start animate?)))
       (when (and animate? (> animate-progress 0.97))
         (level-step level animate?)
         (setf animate? nil))
       (draw-level level-number level animate? animate-progress)))))

#+deploy
(progn (defun sketch::make-default-font ())
       (defun sketch::make-error-font ()))

(defmethod setup ((app rht-game) &key &allow-other-keys)
  (setf *font-face* (load-resource (data-path *font-name*))))

(defmethod kit.sdl2:close-window :before ((app rht-game))
  (setf *running* nil))

(defmethod kit.sdl2:keyboard-event ((app rht-game) state ts repeat? keysym
                                    &aux (key (sdl2:scancode keysym)))
  (when (and (eq state :keydown))
    (with-slots (buffer level level-number appdata animate? animate-start menu) app
      (case buffer
        (:menu (unless animate?
                 (case key
                   (:scancode-m (setf animate? :mute)
                    (setf animate-start (get-internal-real-time)))
                   (:scancode-space
                    (unless (= (menu-level menu) level-number)
                      (setf level-number (menu-level menu))
                      (setf level (level level-number)))
                    (setf buffer :level)))))
        (:level
         (cond
           (animate?)
           ((eql key :scancode-m)
            (setf (menu-level menu) level-number)
            (setf buffer :menu))
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
            (setf animate-start (get-internal-real-time)))))))))

(defun start ()
  (setf *running* t)
  (setf *centered* nil)
  (make-instance 'rht-game)
  #+deploy
  (loop while *running*
        do (sleep 1)))
