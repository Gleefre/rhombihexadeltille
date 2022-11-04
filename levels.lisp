(in-package #:rht/levels)

(defun level-1 ()
  (make-level
   :map          (with-new-map '((0 0))
                   (deftrip '(1/2 0) '(-1/2 1/2) :red))
   :rotation-map (with-new-map ()
                   (defrotate :scancode-s '(0 0) :clock))))

(defun level-2 ()
  (make-level
   :map          (with-new-map '((0 0) (1 0))
                   (deftrip '(1/2 0) '(1/2 1/2) :red)
                   (deftrip '(-2/3 1/3) '(-2/3 1/3) :yellow))
   :rotation-map (with-new-map ()
                   (defrotate :scancode-a '(0 0) :counter-clock)
                   (defrotate :scancode-d '(1 0) :clock))
   :max-steps 10))

(defun level-3 ()
  (make-level
   :map          (with-new-map '((0 0) (1 0))
                   (defbin '(1/2 0))
                   (deftrash '(0 -1/2))
                   (deftrip '(1 -1/2) '(1/2 1/2) :blue))
   :rotation-map (with-new-map ()
                   (defrotate :scancode-s '(0 0) :counter-clock)
                   (defrotate :scancode-w '(1 0) :clock))))

(defun level (n)
  (case n
    (1 (level-1))
    (2 (level-2))
    (3 (level-3))))
