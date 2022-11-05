(in-package #:rht/sketch)

(defparameter *side* 50)
(defparameter *d-side* 5) ; how much space to leave between nodes
(defparameter *mover-r* 5) ; how big do we draw the mover
(defparameter *outside-weight* 5) ;how thick is border of node with goal
(defparameter *font-size* 40) ; for rotation letters
(defparameter *font-shift* 20) ; shift by y-axis to center rotate letter in the node

(defparameter *animate-time* 1/2) ; time to animate rotation

(defparameter *color-map*
  (list (cons :red   +red+  ) (cons :yellow +yellow+)
        (cons :green +green+) (cons :blue   +blue+  )
        (cons :trash +black+)))
(defparameter *key-map*
  '((:scancode-a . :a) (:scancode-s . :s)
    (:scancode-d . :d) (:scancode-w . :w)))
(defparameter *level-map*
  '((:scancode-1 . 1) (:scancode-2 . 2) (:scancode-3 . 3) (:scancode-4 . 4)))

(defun draw-node-outside (hx hy node)
  (destructuring-bind ((n angle) (x y))
      (list (node-shape hx hy) (hex-to-xy hx hy))
    (let ((x (* *side* x))
          (y (* *side* y))
          (side (* (- *side* *d-side*) (ngon-scale n)))
          (sub-side (- (* (- *side* *d-side*) (ngon-scale n))
                       (* 2 *outside-weight*))))
      (with-pen (make-pen :fill +white+)
        (ngon n x y side side angle))
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

(defun draw-node-inside (hx hy node animate? animate-progress hex-map)
  (destructuring-bind ((n angle) (x y))
      (list (node-shape hx hy) (hex-to-xy hx hy))
    (let* ((origin (when animate? (loop for c in (integer-neighbours hx hy)
                                        for n = (gethash c hex-map (make-node))
                                        if (eql animate? (cadr (node-outside n)))
                                        do (return c))))
           (x (* *side* x))
           (y (* *side* y))
           (side (* (- *side* *d-side*) (ngon-scale n))))
      (push-matrix)
      (when origin
        (destructuring-bind (cx cy) (apply #'hex-to-xy origin)
          (let ((angle (if (eql :clock (caddr (node-outside (gethash origin hex-map)))) 60 -60)))
            (rotate (* angle animate-progress) (* cx *side*) (* cy *side*))
            (decf side (* *d-side* 2 (sin (* pi animate-progress)))))))
      (with-pen (make-pen :fill (rgb 0 0 0 0.2))
        (ngon n x y side side angle))
      (when (node-inside node)
        (with-pen (make-pen :fill (cdr (assoc (node-inside node) *color-map*)))
          (circle x y *mover-r*)))
      (pop-matrix))))

(defun get-animate-progress (animate-start animate?)
  (when animate?
    (/ (- (get-internal-real-time) animate-start) internal-time-units-per-second
       *animate-time*)))

(defsketch draw-level ((level-number 1)
                       (level (level 1))
                       (animate? nil)
                       (animate-start 0))
  (background (rgb 0.5 0.3 0.6))
  (translate 300 300)
  (maphash (lambda (c node) (draw-node-outside (car c) (cadr c) node))
           (level-hexagon-map level))
  (let ((progress (get-animate-progress animate-start animate?)))
    (when (and animate? (> progress 0.95))
      (level-step level animate?)
      (setf animate? nil))
    (maphash (lambda (c node) (draw-node-inside (car c) (cadr c) node
                                                animate? progress (level-hexagon-map level)))
             (level-hexagon-map level)))
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
  (when (and (eq state :keydown))
    (with-slots (level level-number appdata animate? animate-start) app
      (cond
        (animate?)
        ((eql key :scancode-r) (setf level (level level-number)))
        ((assoc key *level-map*) (setf level-number (cdr (assoc key *level-map*))
                                       level (level level-number)))
        ((not (eql :play (level-state level))))
        ((assoc key *key-map*)
         (setf animate? (cdr (assoc key *key-map*)))
         (setf animate-start (get-internal-real-time)))))))

(defun start ()
  (make-instance 'draw-level :width 600 :height 600))
