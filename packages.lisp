(defpackage #:rhombihexadeltille/core
  (:use #:cl)
  (:export #:node
           #:make-level-map
           #:rotate
           #:key-rotate
           #:with-map
           #:deftrip
           #:defrotate
           #:deftrash
           #:defbin
           #:win?)
  (:nicknames #:rht/core))

(defpackage #:rhombihexadeltille/levels
  (:use #:cl #:rht/core)
  (:export #:level)
  (:nicknames #:rht/levels))
