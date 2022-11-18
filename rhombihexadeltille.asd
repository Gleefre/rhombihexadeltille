(asdf:defsystem "rhombihexadeltille"
  :description "A little puzzle game"
  :version "0.0.7"
  :author "Gleefre <varedif.a.s@gmail.com>"
  :licence "Apache 2.0"
  :depends-on ("sketch" "sdl2-mixer")
  :pathname "src"
  :components ((:file "packages")
               (:file "geometry")
               (:file "core")
               (:file "levels")
               (:file "utils")
               (:file "conf")
               (:file "sketch-utils")
               (:file "sketch"))

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "rhombihexadeltille-game"
  :entry-point "rht:start")
