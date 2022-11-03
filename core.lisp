(in-package #:rht/core)

;; Node is a single ngon
;; It can contain a mover inside and a goal outside

(defstruct node
  outside inside)

;; Functions to manipulate map of nodes

(defun neighbours (x y)
  (let ((shifts '((0  1/2) ( 1/3  1/3) ( 1/2 0) ( 2/3 -1/3) ( 1/2 -1/2) ( 1/3 -2/3)
                  (0 -1/2) (-1/3 -1/3) (-1/2 0) (-2/3  1/3) (-1/2  1/2) (-1/3  2/3))))
    (loop for (dx dy) in shifts
          collect (list (+ x dx) (+ y dy)))))

(defun add-hexagon (map hx hy)
  (unless (gethash (list hx hy) map)
    (setf (gethash (list hx hy) map) (make-node))
    (loop for coords in (neighbours hx hy)
          unless (gethash coords map)
          do (setf (gethash coords map) (make-node)))))

(defun make-level-map (hexagons)
  (let ((level-map (make-hash-table :test 'equal)))
    (loop for (hx hy) in hexagons
          do (add-hexagon level-map hx hy))
    level-map))

(defmacro rotate-insides (list-name length start k)
  `(rotatef ,@(loop for i from start below length by k
                    collect `(node-inside (nth ,i ,list-name)))))

(defun hexagon-rotate (map x y)
  (let ((nodes (loop for c in (neighbours x y)
                     collect (gethash c map))))
    (rotate-insides nodes 12 0 2)
    (rotate-insides nodes 12 1 2)))

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
  `(push (list ,coords ,direction) (gethash ,key map nil)))

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
          if (eql direction :clockwise)
          do (hexagon-rotate map x y)
          if (eql direction :counterclockwise)
          do (loop repeat 5
                   do (rotate map x y)))))

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
    (when (eql state :play) ; step further only if it is playable
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
