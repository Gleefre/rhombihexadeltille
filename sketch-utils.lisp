(in-package #:rht/sketch/utils)

;; Basic with- macros for translate, rotate and scale sketch functions

(defmacro with-translate ((dx dy) &body body)
  `(progn
     (push-matrix)
     (translate ,dx ,dy)
     ,@body
     (pop-matrix)))

(defmacro with-rotate ((angle &optional (cx 0) (cy 0)) &body body)
  `(progn
     (push-matrix)
     (rotate ,angle ,cx ,cy)
     ,@body
     (pop-matrix)))

(defmacro with-scale ((sx &optional sy (cx 0) (cy 0)) &body body)
  `(progn
     (push-matrix)
     (scale ,sx ,sy ,cx ,cy)
     ,@body
     (pop-matrix)))

;; Fit -- function to fit desired width/height to rectangle on screen

(defun fit (width height from-width from-height &optional (to-x 0) (to-y 0) (from-x 0) (from-y 0) max-scale)
  (translate from-x from-y)
  (let* ((scale (min (/ from-width width)
                     (/ from-height height)
                     (if max-scale max-scale
                         (/ from-height height))))
         (x-shift (/ (- from-width (* width scale)) 2))
         (y-shift (/ (- from-height (* height scale)) 2)))
    (translate x-shift y-shift)
    (scale scale))
  (translate (- to-x) (- to-y)))

(defmacro with-fit ((width height from-width from-height
                     &optional  (to-x 0) (to-y 0) (from-x 0) (from-y 0) max-scale)
                    &body body)
  `(progn
     (push-matrix)
     (fit ,width ,height ,from-width ,from-height ,to-x ,to-y ,from-x ,from-y ,max-scale)
     ,@body
     (pop-matrix)))
