(defpackage #:rhombihexadeltille/core
  (:use #:cl)
  (:export #:node #:node-inside #:node-outside
           #:make-level-map
           #:rotate #:key-rotate
           #:with-map #:deftrip #:defrotate #:deftrash #:defbin
           #:win?)
  (:nicknames #:rht/core))

(defpackage #:rhombihexadeltille/levels
  (:use #:cl #:rht/core)
  (:export #:level)
  (:nicknames #:rht/levels))

(defpackage #:rhombihexadeltille/draw
  (:use #:cl #:rht/core #:rht/levels)
  (:nicknames #:rht/draw)
  (:import-from #:sketch
                #:defsketch #:translate #:with-pen #:make-pen
                #:ngon #:circle #:text
                #:+red+ #:+green+ #:+yellow+ #:+blue+ #:+black+))
