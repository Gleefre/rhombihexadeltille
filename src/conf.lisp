(in-package #:rht/conf)

(defstruct conf muted results (menu-level 1))

(defun load-conf (filename &aux (pathname (probe-file filename)))
  (if pathname
      (with-open-file (in pathname :direction :input)
        (with-standard-io-syntax
          (read in)))
      (make-instance 'conf)))

(defun save-conf (conf filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print conf out))))

(defun muted? (conf)
  (conf-muted conf))

(defun (setf muted?) (new-value conf)
  (setf (conf-muted conf) new-value))

(defun level-passed? (level conf)
  (cdr (assoc level (conf-results conf))))

(defun (setf level-passed?) (new-value level conf)
  (if (cdr (assoc level (conf-results conf)))
      (setf (cdr (assoc level (conf-results conf)))
            (min (cdr (assoc level (conf-results conf)))
                 new-value))
      (push (cons level new-value) (conf-results conf))))
