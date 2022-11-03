(in-package #:rht/levels)

(defun level-1 ()
  (with-map '((0 0))
    (deftrip '(1/2 0) '(-1/2 1/2) :red)))

(defun level (n)
  (case n
    (1 (level-1))))
