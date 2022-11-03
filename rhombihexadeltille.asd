(defsystem "rhombihexadeltille"
  :description "A little puzzle game"
  :version "0.0.0"
  :author "Gleefre <varedif.a.s@gmail.com>"
  :depends-on ("sketch")
  :components ((:file "packages")
               (:file "core")
               (:file "levels")
               (:file "draw")))
