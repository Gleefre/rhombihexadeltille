(in-package #:rht/core)

;; Node is a single ngon

(defstruct node
  outside inside)

;; Functions to manipulate map of nodes

(defun neighbours (x y)
  (let ((shifts '((0  1/2) ( 1/3  1/3) ( 1/2 0) ( 2/3 -1/3) ( 1/2 -1/2) ( 1/3 -2/3)
                  (0 -1/2) (-1/3 -1/3) (-1/2 0) (-2/3  1/3) (-1/2  1/2) (-1/3  2/3))))
    (loop for (dx dy) in shifts
          collect (list (+ x dx) (+ y dy)))))

(defun make-level-map (hexagons)
  (let ((level-map (make-hash-table :test 'equal)))
    (loop for (hx hy) in hexagons
          do (setf (gethash (list hx hy) level-map) (make-node))
          do (loop for coords in (neighbours hx hy)
                   unless (gethash coords level-map)
                   do (setf (gethash coords level-map) (make-node))))
    level-map))

(defmacro rotate-insides (list-name length start k)
  `(rotatef ,@(loop for i from start below length by k
                    collect `(node-inside (nth ,i ,list-name)))))

(defun rotate (map x y)
  (let ((nodes (loop for c in (neighbours x y)
                     collect (gethash c map))))
    (rotate-insides nodes 12 0 2)
    (rotate-insides nodes 12 1 2)))

(defun key-rotate (map key)
  (loop for ((x y) direction) in (gethash key map)
        if (eql direction :clockwise)
        do (rotate map x y)
        if (eql direction :counterclockwise)
        do (loop repeat 3
                 do (rotate map x y))))

;; Functions to define map

(defmacro with-map (initial-coords &body body)
  `(let ((map (make-level-map ,initial-coords)))
     ,@body
     map))

(defmacro deftrip (depart arrive color)
  `(setf (node-inside  (gethash ,depart map)) ,color
         (node-outside (gethash ,arrive map)) ,color))

(defmacro defrotate (key coords direction)
  `(push (list ,coords ,direction) (gethash ,key map nil)))

(defmacro deftrash (coords)
  `(setf (node-inside (gaethash ,coords map)) :trash))

(defmacro defbin (coords)
  `(setf (node-outside (gaethash ,coords map)) :bin))

;; Functions to check if the level was passed

(defun arrived? (node)
  (case (node-inside node)
    ((:red :green :yellow :blue) (eql (node-inside node) (node-outside node)))
    (:trash nil)))

(defun win? (map)
  (loop for node being the hash-values of map
        always (arrived? node)))
