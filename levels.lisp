(in-package #:rht/levels)

(defun level-1 ()
  (with-new-level
    (defl/hex (0 0))
    (defl/trip :red (:e 0 0) (-1/2 1/2))
    (defl/rotate :s :clock (0 0))))

(defun level-2 ()
  (with-new-level
    (defl/hex ((0 0) (1 0)))
    (defl/trip :red (1/2 0) (1/2 1/2))
    (defl/trip :yellow (-2/3 1/3) (-2/3 1/3))
    (defl/rotate :a :counter-clock (0 0))
    (defl/rotate :d :clock (1 0))
    (defl/maxsteps 10)))

(defun level-3 ()
  (with-new-level
    (defl/hex ((0 0) (1 0) (0 1) (1 1)))
    (defl/bin (1/2 0) nil)
    (defl/trash (0 -1/2))
    (defl/trip :blue (1 -1/2) (1/2 1/2))
    (defl/rotate :s :counter-clock (0 0))
    (defl/rotate :w :clock (1 0))))

(defun level (n)
  (case n
    (1 (level-1))
    (2 (level-2))
    (3 (level-3))))
