(in-package #:rhombihexadeltille/core)

(defstruct node
  outside inside)

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
