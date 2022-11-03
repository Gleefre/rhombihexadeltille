(in-package #:rht/levels)

(defun level-1 ()
  (with-map '((0 0))
    (deftrip '(1/2 0) '(-1/2 1/2) :red)))

(defun level-2 ()
  (with-map '((0 0) (1 0))
    (deftrip '(1/2 0) '(1/2 1/2) :red)
    (deftrip '(-2/3 1/3) '(-2/3 1/3) :yellow)))

(defun level (n)
  (case n
    (1 (level-1))
    (2 (level-2))))
