(in-package #:rht/levels)

(defun level-1 ()
  (with-new-level
    (defl/hex (0 0))
    (defl/rotate :1 :clock (0 0))
    (defl/trip :blue (:e 0 0) (:w 0 0))
    (defl/text "~%Press 1 to rotate the hexagon")))

(defun level-2 ()
  (with-new-level
    (defl/hex ((0 0) (1 0) (2 0)))
    (defl/text "~%Sometimes hexagons cannot be rotated.")
    (defl/rotate :1 :clock (2 0))
    (defl/rotate :1 :counter-clock (0 0))
    (defl/trip :green ((:nne 1 0) (:nnw 1 0)) ((:nne 1 0) (:nnw 1 0)))
    (defl/trip :red ((:n 2 0) (:n 0 0)) ((:s 2 0) (:s 0 0)))
    (defl/trip :blue ((:w 2 0) (:e 0 0)) ((:e 2 0) (:w 0 0)))))

(defun level-3 ()
  (with-new-level
    (defl/text "~%Sometimes you need to be fast!")
    (defl/hex ((0 0) (1 0)))
    (defl/rotate :1 :clock (0 0))
    (defl/rotate :2 :counter-clock (1 0))
    (defl/trip :red ((:e 0 0) (:ene 0 0)) ((:e 0 0) (:ese 0 0)))
    (defl/maxsteps 20)))

(defun level-4 ()
  (with-new-level
    (defl/hex ((0 0) (1 0) (0 1)))
    (defl/text (defl/text "Levels can be hard.~%But you can skip them! (N)"))
    (let ((reds (mapcar (lambda (x) (hex-> x 0 0))
                        '(:nne :n :nnw :wnw :w :wsw :ssw :s :sse)))
          (blues (mapcar (lambda (x) (hex-> x 0 1))
                         '(:w :wsw :ssw :s :sse :ese :e :ene :nne)))
          (greens (mapcar (lambda (x) (hex-> x 1 0))
                          '(:sse :ese :e :ene :nne :n :nnw :wnw :w))))
      (defl/trip :red reds blues)
      (defl/trip :blue blues greens)
      (defl/trip :green greens reds))
    (defl/rotate :1 :clock (0 0))
    (defl/rotate :2 :clock (1 0))
    (defl/rotate :3 :clock (0 1))))

(defun level-5 ()
  (with-new-level
    (let* ((hexs '((-1 1) (0 0) (1 0) (1 1) (0 2) (-1 2) (0 1)))
           (greens (loop for dir in +sq-directions+
                         collect (hex-> dir 0 1)))
           (greens-dest (loop for dir in +sq-directions+
                              for (x y) in hexs
                              collect (hex-> dir x y))))
      (defl/hex hexs)
      (defl/trip :green greens greens-dest))
    (defl/rotate :1 :clock (0 0))
    (defl/rotate :2 :clock (1 0))
    (defl/rotate :1 :clock (1 1))
    (defl/rotate :2 :clock (0 2))
    (defl/rotate :1 :clock (-1 2))
    (defl/rotate :2 :clock (-1 1))))

(defun level-6 ()
  (with-new-level
    (defl/hex ((0 0) (1 0) (2 0)))
    (defl/rotate :1 :clock (0 0))
    (defl/rotate :1 :counter-clock (2 0))
    (defl/rotate :2 :clock (1 0))
    (defl/trip :blue
      ((:wnw 0 0) (:n 0 0) (:ene 0 0) (:n 1 0) (:ene 1 0) (:n 2 0) (:ene 2 0))
      ((:wsw 0 0) (:s 0 0) (:ese 0 0) (:s 1 0) (:ese 1 0) (:s 2 0) (:ese 2 0)))))

(defun level-7 ()
  (with-new-level
    (defl/hex ((0 0) (1 0) (-1 2) (0 2) (0 1)))
    (defl/trip :red
      ((:wnw 0 0) (:n 0 0) (:ene 0 0) (:n 1 0) (:ene 1 0))
      ((:wsw -1 2) (:s -1 2) (:wsw 0 2) (:s 0 2) (:ese 0 2)))
    (defl/rotate :1 :clock ((0 0) (0 2)))
    (defl/rotate :3 :counter-clock ((1 0) (-1 2)))
    (defl/rotate :2 :clock (0 1))))

(defun level-8 ()
  (with-new-level
    (defl/hex ((0 0) (1 0)))
    (let ((greens (loop for dir in '(:nnw :wnw :w :wsw :ssw)
                        collect (hex-> dir 0 0)))
          (blues (loop for dir in '(:nne :ene :e :ese :sse)
                       collect (hex-> dir 1 0))))
      (defl/trip :green greens blues)
      (defl/trip :blue blues greens))
    (defl/rotate :1 :clock (1 0))
    (defl/rotate :2 :counter-clock (0 0))))

(defun level-9 ()
  (with-new-level
    (defl/hex ((0 0)))
    (defl/bin ((:ene 0 0) (:e 0 0)) nil)
    (defl/trash ((:n 0 0) (:nnw 0 0) (:wnw 0 0) (:w 0 0) (:wsw 0 0) (:ssw 0 0) (:s 0 0)))
    (defl/rotate :1 :counter-clock (0 0))
    (defl/text "~%Throw all black points to bin!")))

(defun level-10 ()
  (with-new-level
    (defl/hex ((0 0) (1 0) (0 1)))
    (defl/trash ((:s 0 0) (:s 1 0) (:s 0 1)))
    (defl/bin (:ese 0 0) nil)
    (defl/trip :green (:n 1 0) (:n 0 0))
    (defl/text "Be careful!~%Don't throw away this green point!")
    (defl/rotate :1 :clock (0 0))
    (defl/rotate :2 :clock (1 0))
    (defl/rotate :3 :clock (0 1))))

(defun level-11 ()
  (with-new-level
    (let* ((hexs '((0 0) (1 0) (0 1) (-1 1) (-1 0) (0 -1) (1 -1)))
           (trash (loop for (hx hy) in hexs
                        append (loop for dir in +sq-directions+
                                     collect (hex-> dir hx hy))))
           (bins (loop for dir in +sq-directions+
                       collect (hex-> dir 0 0)))
           (trash (set-difference trash bins :test 'equal)))
      (defl/hex hexs)
      (defl/trash trash)
      (defl/bin (:w 0 0) nil)
      (defl/rotate :2 :counter-clock ((1 0) (-1 1) (0 -1)))
      (defl/rotate :1 :clock ((0 1) (-1 0) (1 -1)))
      (defl/maxsteps 45))))

(defun level-12 ()
  (with-new-level
    (defl/hex ((1 0) (2 0) (3 0) (0 2) (1 2) (2 2)))
    (defl/rotate :1 :clock ((1 0) (0 2)))
    (defl/rotate :2 :clock ((2 0) (1 2)))
    (defl/rotate :3 :clock ((3 0) (2 2)))
    (defl/trip :green ((:ese 1 0) (:e 1 0) (:e 2 0)) ((:ene 1 0) (:e 1 0) (:e 2 0)))
    (defl/trip :red ((:ese 1 2) (:e 1 2) (:e 0 2)) ((:ene 1 2) (:e 1 2) (:e 0 2)))))

(defun level-13 ()
  (with-new-level
    (defl/hex ((0 0) (2 0) (-1 2) (1 2)))
    (loop for (x y) in '((0 0) (2 0) (-1 2) (1 2))
          do (defl/trip :blue (:n x y) (:s x y))
          do (defl/trip :green (:s x y) (:n x y))
          do (defl/rotate :1 :clock (:c x y)))
    (defl/text "You have passed all levels!~%Congratulations!")))

(defparameter *levels* 12)

(defun level (n)
  (case n
    (1 (level-1))
    (2 (level-2))
    (3 (level-3))
    (4 (level-4))
    (5 (level-5))
    (6 (level-6))
    (7 (level-7))
    (8 (level-8))
    (9 (level-9))
    (10 (level-10))
    (11 (level-11))
    (12 (level-12))
    (13 (level-13))))
