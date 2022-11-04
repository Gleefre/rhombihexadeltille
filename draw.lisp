(in-package #:rht/draw)

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

(defun draw-node (x y node side)
  (destructuring-bind (n angle) (node-shape x y)
    (destructuring-bind (x y) (hex-to-xy x y)
      (let ((x (* side x))
            (y (* side y))
            (side (- (* side (ngon-scale n)) 2)))
        (ngon n x y side side angle)
        (when (node-inside node)
          (with-pen (make-pen :fill (case (node-inside node)
                                      (:red +red+)
                                      (:yellow +yellow+)
                                      (:green +green+)
                                      (:blue +blue+)
                                      (:trash +black+)))
            (circle x y 5)))
        (when (node-outside node)
          (with-pen (make-pen :stroke (case (node-outside node)
                                        (:red +red+)
                                        (:yellow +yellow+)
                                        (:green +green+)
                                        (:blue +blue+)
                                        (:bin +black+))
                              :weight 5)
            (ngon n x y (- side 10) (- side 10) angle)))))))

(defun draw-rotation (key config &aux (key (case key
                                             (:scancode-s "S")
                                             (:scancode-a "A")
                                             (:scancode-d "D")
                                             (:scancode-w "W"))))
  (loop for ((hx hy) dir) in config
        for (x y) = (hex-to-xy hx hy)
        do (with-font (make-font :size 40 :color (case dir
                                                   (:clockwise +blue+)
                                                   (:counterclockwise +blue+))
                                 :align :center)
             (text key (* 50 x) (- (* 50 y) 20)))))

(defsketch draw-level ((level-number 1)
                       (level (level 1)))
  (translate 300 300)
  (maphash (lambda (c node) (draw-node (car c) (cadr c) node 50))
           (level-map level))
  (mapcar (lambda (key &aux (coords (gethash key (level-rotation-map level))))
            (draw-rotation key coords))
          (gethash :rotations (level-rotation-map level)))
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
      (case key
        ((:scancode-s :scancode-a :scancode-d :scancode-w)
         (level-step level key))
        (:scancode-1 (setf level (level 1)
                           level-number 1))
        (:scancode-2 (setf level (level 2)
                           level-number 2))
        (:scancode-3 (setf level (level 3)
                           level-number 3))))))

(make-instance 'draw-level :width 600 :height 600)
