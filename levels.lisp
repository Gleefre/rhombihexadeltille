(in-package #:rht/levels)

(defun level-1 ()
  (make-level
   :map          (with-new-map '((0 0))
                   (deftrip '(1/2 0) '(-1/2 1/2) :red))
   :rotation-map (with-new-map ()
                   (defrotate :space '(0 0) :clockwise))))

(defun level-2 ()
  (make-level
   :map          (with-new-map '((0 0) (1 0))
                   (deftrip '(1/2 0) '(1/2 1/2) :red)
                   (deftrip '(-2/3 1/3) '(-2/3 1/3) :yellow))
   :rotation-map (with-new-map ()
                   (defrotate :a '(0 0) :clockwise)
                   (defrotate :b '(1 0) :counterclockwise))))

(defun level (n)
  (case n
    (1 (level-1))
    (2 (level-2))))
