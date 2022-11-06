(in-package #:rht/utils)

;; data-path based on deploy:data-directory

(defun data-path (relative-path)
  (format nil "~a"
          #-deploy (asdf:system-relative-pathname "rhombihexadeltille"
                                                  relative-path)
          #+deploy (merge-pathnames (deploy:data-directory) relative-path)))

;; do-later and do-now

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
