(in-package #:rht/draw)

(defparameter *side* 50)
(defparameter *d-side* 5) ; how much space to leave between nodes
(defparameter *mover-r* 5) ; how big do we draw the mover
(defparameter *outside-weight* 5) ;how thick is border of node with goal
(defparameter *font-size* 40) ; for rotation letters
(defparameter *font-shift* 20) ; shift by y-axis to center rotate letter in the node

(defparameter *color-map*
  (list (cons :red   +red+  ) (cons :yellow +yellow+)
        (cons :green +green+) (cons :blue   +blue+  )
        (cons :trash +black+)))
(defparameter *key-map*
  '((:scancode-a . :a) (:scancode-s . :s)
    (:scancode-d . :d) (:scancode-w . :w)))
(defparameter *level-map*
  '((:scancode-1 . 1) (:scancode-2 . 2) (:scancode-3 . 3)))

(defun node-shape (x y &aux (dx (mod x 1)) (dy (mod y 1)))
  (case (+ dx (* 3 dy))
    (  0 '(6  90))
    (1/2 '(4  45))
    (3/2 '(4  15))
    (  2 '(4  75))
    (4/3 '(3  90))
    (8/3 '(3 270))))

(defun ngon-scale (n)
  (case n
    (4 (sqrt 1/2))
    (3 (sqrt 1/3))
    (6          1)))

(defun draw-node (x y node)
  (destructuring-bind ((n angle) (x y))
      (list (node-shape x y) (hex-to-xy x y))
    (let ((x (* *side* x))
          (y (* *side* y))
          (side (* (- *side* *d-side*) (ngon-scale n)))
          (sub-side (- (* (- *side* *d-side*) (ngon-scale n))
                       (* 2 *outside-weight*))))
      (ngon n x y side side angle)
      (when (node-inside node)
        (with-pen (make-pen :fill (cdr (assoc (node-inside node) *color-map*)))
          (circle x y *mover-r*)))
      (case (car (node-outside node))
        (:color  (with-pen (make-pen :stroke (cdr (assoc (second (node-outside node)) *color-map*))
                                     :weight *outside-weight*)
                   (ngon n x y sub-side sub-side angle)))
        (:bin    (with-pen (make-pen :stroke (gray (if (cadr (node-outside node)) 0.5 0))
                                     :weight *outside-weight*)
                   (ngon n x y sub-side sub-side angle)))
        (:rotation (with-font (make-font :size *font-size* :color (case (third (node-outside node))
                                                                    (:clock +blue+)
                                                                    (:counter-clock +red+))
                                         :align :center)
                     (text (format nil "~a" (second (node-outside node)))
                           x (- y *font-shift*))))))))

(defsketch draw-level ((level-number 1)
                       (level (level 1)))
  (translate 300 300)
  (maphash (lambda (c node) (draw-node (car c) (cadr c) node))
           (level-hexagon-map level))
  (translate -300 -300)
  (with-font (make-font :align :left :size 50)
    (text (format nil "level: ~a" level-number) 300 0)
    (text (format nil "steps: ~a ~{/ ~a~}" (level-steps level)
                  (unless (zerop (level-max-steps level)) (list (level-max-steps level))))
          300 50))
  (with-font (make-font :color (gray .8) :size 50)
    (text (case (level-state level)
            (:won "Hurray! :)")
            (:play "Hm... What do I do?")
            (:lost "x-x"))
          100 100)))

(defmethod kit.sdl2:keyboard-event ((app draw-level) state ts repeat? keysym
                                    &aux (key (sdl2:scancode keysym)))
  (when (and (eq state :keydown) (not repeat?))
    (with-slots (level level-number) app
      (cond
        ((eql key :scancode-r) (setf level (level level-number)))
        ((assoc key *key-map*) (level-step level (cdr (assoc key *key-map*))))
        ((assoc key *level-map*) (setf level-number (cdr (assoc key *level-map*))
                                       level (level level-number)))))))

(make-instance 'draw-level :width 600 :height 600)
