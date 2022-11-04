(in-package #:rht/levels)

(defun level-1 ()
  (with-new-level
    (defl/hex (0 0))
    (defl/trip :red (:e 0 0) (:ssw 0 0))
    (defl/rotate :s :clock (0 0))))

(defun level-2 ()
  (with-new-level
    (defl/hex ((0 0) (1 0)))
    (defl/trip :red ((:wnw 1 0) (:w 1 0)) ((:s 1 0) (:ssw 1 0)))
    (defl/trip :yellow (-2/3 1/3) (-2/3 1/3))
    (defl/rotate :a :counter-clock (0 0))
    (defl/rotate :d :clock (1 0))
    (defl/maxsteps 100)))

(defun level-3 ()
  (with-new-level
    (defl/hex ((0 0) (1 0) (0 1)))
    (defl/bin (:w 1 0) nil)
    (defl/bin (:e 0 1) t)
    (defl/trash (0 -1/2))
    (defl/trip :blue (:nnw 1 0) (:ssw 1 0))
    (defl/rotate :s :counter-clock (0 0))
    (defl/rotate :w :clock (1 0))
    (defl/rotate :d :clock (0 1))))

(defun level-4 ()
  (with-new-level
    (defl/hex ((0 0) (1 0) (0 1)))
    (let ((yellow (mapcar (lambda (x) (hex-> x 0 0))
                          '(:nne :n :nnw :wnw :w :wsw :ssw :s :sse)))
          (red (mapcar (lambda (x) (hex-> x 0 1))
                       '(:w :wsw :ssw :s :sse :ese :e :ene :nne)))
          (blue (mapcar (lambda (x) (hex-> x 1 0))
                        '(:sse :ese :e :ene :nne :n :nnw :wnw :w))))
      (defl/trip :green yellow red)
      (defl/trip :red red blue)
      (defl/trip :blue blue yellow))
    (defl/rotate :s :clock (0 0))
    (defl/rotate :w :clock (1 0))
    (defl/rotate :d :clock (0 1))))

(defun level (n)
  (case n
    (1 (level-1))
    (2 (level-2))
    (3 (level-3))
    (4 (level-4))))
