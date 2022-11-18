(in-package :rht/geometry)

;; Point can be found by looking in some direction from another point
;; direction is one of (:n :nne :ene :e :ese :sse :s :ssw :wsw :w :wnw :nnw :c)
;; where :c means don't move at all

(defparameter +all-directions+ '(:c :n :nne :ene :e :ese :sse :s :ssw :wsw :w :wnw :nnw))
(defparameter +directions+     '(   :n :nne :ene :e :ese :sse :s :ssw :wsw :w :wnw :nnw))
(defparameter +t-directions+   '(   :n      :ene    :ese      :s      :wsw    :wnw     ))
(defparameter +sq-directions+  '(      :nne      :e      :sse    :ssw      :w      :nnw))

(defun c+ (p1 p2)
  (list (+ (first p1) (first p2))
        (+ (second p1) (second p2))))

(defun hex-> (direction &rest h-xy)
  (c+ h-xy
      (case direction
        (:n '( 1/3 -2/3)) (:nne '( 1/2 -1/2)) (:ene '( 2/3 -1/3))
        (:e '( 1/2    0)) (:ese '( 1/3  1/3)) (:sse '(   0  1/2))
        (:s '(-1/3  2/3)) (:ssw '(-1/2  1/2)) (:wsw '(-2/3  1/3))
        (:w '(-1/2    0)) (:wnw '(-1/3 -1/3)) (:nnw '(   0 -1/2))
        (:c '(   0    0)))))

;; We want to be able to find all hexagons attached to this node

(defun integer-neighbours (x y)
  (loop for dir in +all-directions+
        for coords = (hex-> dir x y)
        when (zerop (+ (mod (car coords) 1) (mod (cadr coords) 1)))
        collect coords))

;; We want to transform coordinates to/from our base to usual base

(defun matrix-multiply (matrix vector)
  (loop for col in matrix
        collect (reduce #'+ (mapcar #'* col vector))))

(defparameter +transform-to+
  `((,(+ 1 (sqrt 3)) ,(+ 1/2 (sqrt 3/4)))
    (0               ,(+ 3/2 (sqrt 3/4)))))

(defparameter +transform-back+
  `((,(- (sqrt 3/4) 1/2) ,(- (sqrt 1/12) 1/2))
    (0                   ,(- 1 (sqrt 1/3)))))

(defun hex-to-xy (&rest coords)
  (matrix-multiply +transform-to+   coords))

(defun xy-to-hex (&rest coords)
  (matrix-multiply +transform-back+ coords))

;; We want to know shape and radius scale for nodes

(defun node-shape (x y &aux (dx (mod x 1)) (dy (mod y 1)))
  "Returns a list (n angle) where n is number of vertices and angle is a rotation angle"
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
    (6       1.075)))

;; We want to bound hexagon-map to draw it at center and fit the window

(defun bounds (hexagon-map)
  "Returns x y width height of hexagon map (in real coords)"
  (loop for (hx hy) being the hash-key of hexagon-map
        for (x y) = (hex-to-xy hx hy)
        when (and (integerp hx) (integerp hy))
        maximize x into max-x and minimize x into min-x and
        maximize y into max-y and minimize y into min-y
        finally (return (list (- min-x 2)        (- min-y 2)
                              (- max-x min-x -4) (- max-y min-y -4)))))
