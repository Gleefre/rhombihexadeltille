(defsystem "rhombihexadeltille"
  :description "A little puzzle game"
  :version "0.0.3"
  :author "Gleefre <varedif.a.s@gmail.com>"
  :licence "Apache 2.0"
  :depends-on ("sketch")
  :components ((:file "packages")
               (:file "geometry")
               (:file "core")
               (:file "levels")
               (:file "utils")
               (:file "sketch-utils")
               (:file "sketch"))
