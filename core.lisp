(in-package #:rht/core)

;; Node is a single ngon
;; It can contain a mover inside and a goal/actor outside

;; A mover is one of nil, :trash or keyword for color
;; Possible colors: (:red :yellow :green :blue)
;; To add colors you need to add it to *color-map* variable in rht/draw package

;; A goal is one of colors
;; An actor is one of (:bin safe?) or (:rotate key direction)
;; safe? is NIL or T, depending on is it safe for movers to stay there
;; direction is one of :clock or :counter-clock
;; Possible keys: (:a :s :w :d)
;; To add keys you need to add it to *key-map* variable in rht/draw package

(defstruct node
  outside inside)

;; Functions to manipulate map of nodes

(defun add-hexagon (map hx hy)
  (loop for dir in +all-directions+
        for point = (hex-> dir hx hy)
        do (setf (gethash point map)
                 (gethash point map (make-node)))))

(defun make-level-map (hexagons)
  (let ((level-map (make-hash-table :test 'equal)))
    (loop for (hx hy) in hexagons
          do (add-hexagon level-map hx hy))
    level-map))

(defmacro rotate-insides (list-name length start k)
  `(rotatef ,@(loop for i = start then (+ i k)
                    repeat (/ length (gcd length (abs k)))
                    collect `(node-inside (nth ,(mod i length) ,list-name)))))

(defun hexagon-rotate (map x y direction)
  (let ((triangles (loop for dir in +t-directions+
                         collect (gethash (hex-> dir x y) map)))
        (squares   (loop for dir in +sq-directions+
                         collect (gethash (hex-> dir x y) map))))
    (if (eql direction :clock)
        (progn (rotate-insides squares   6 0 -1)
               (rotate-insides triangles 6 0 -1))
        (progn (rotate-insides squares   6 0  1)
               (rotate-insides triangles 6 0  1)))))

;; Macros to define map (works both for map and rotation-map

(defmacro with-new-map (initial-coords &body body)
  `(let ((map (make-level-map ,initial-coords)))
     ,@body
     map))

(defmacro with-map (map-name &body body)
  `(let ((map ,map-name))
     ,@body
     map))

;; Macros to build map

(defmacro deftrip (depart arrive color)
  `(setf (node-inside  (gethash ,depart map)) ,color
         (node-outside (gethash ,arrive map)) ,color))

(defmacro deftrash (coords)
  `(setf (node-inside (gethash ,coords map)) :trash))

(defmacro defbin (coords)
  `(setf (node-outside (gethash ,coords map)) :bin))

(defmacro defhex (coords)
  `(apply #'add-hexagon map ,coords))

;; Macro to build rotation map

(defmacro defrotate (key coords direction)
  `(progn (push (list ,coords ,direction) (gethash ,key map nil))
          (pushnew ,key (gethash :rotations map nil))))

;; Level consists of map, rotation map, number of steps done, steps restriction and win/lose/play state

(defstruct level
  map rotation-map
  (steps 0)
  (max-steps 0) ; 0 means no restriction
  (state :play)) ; :won / :lost / :play

;; Make a rotation by its name

(defun key-rotate (level key)
  (with-slots (map rotation-map) level
    (loop for ((x y) direction) in (gethash key rotation-map)
          do (hexagon-rotate map x y direction))))

;; Throw all to the bins
;; You lost if a mover is thrown to the bin

(defun throw-to-bins (level)
  (maphash (lambda (_ node)
             (declare (ignore _))
             (when (eql (node-outside node) :bin)
               (when (node-inside node)
                 (case (node-inside node)
                   (:trash (setf (node-inside node) nil))
                   (t (setf (level-state level) :lost))))))
           (level-map level)))

;; Making a step
;; We need to (1) count a step, then (2) make a rotation,
;; then to (3) throw trash to the bins, then (4) update win/lose state

(defun level-step (level rotation-key)
  (with-slots (state steps max-steps) level
    (when (and (eql state :play) ; step further only if it is playable
               (member rotation-key
                       (gethash :rotations (level-rotation-map level))))
      (incf steps)
      (key-rotate level rotation-key)
      (throw-to-bins level)
      (cond ((win? level) (setf state :won))
            ((= steps max-steps) (setf state :lost))))))

;; Functions to check if the level was passed

(defun arrived? (node)
  (case (node-inside node)
    ((:red :green :yellow :blue) (eql (node-inside node) (node-outside node)))
    (:trash nil)
    (t t)))

(defun all-arrived? (map)
  (loop for node being the hash-values of map
        always (arrived? node)))

(defun win? (level)
  (with-slots (map steps max-steps) level
    (and (all-arrived? map)
         (or (zerop max-steps)
             (<= steps max-steps)))))
