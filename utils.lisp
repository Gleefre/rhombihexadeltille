(in-package #:rht/utils)

(defparameter *table* ())
(defmacro do-later (priority &body body)
  `(push (cons ,priority
               (lambda ()
                 ,@body))
         *table*))
(defmacro do-now ()
  `(progn
     (loop for (p . function) in (sort *table* #'< :key #'car)
           do (funcall function))
     (setf *table* ())))
