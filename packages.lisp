(defpackage #:rhombihexadeltille/core
  (:use #:cl)
  (:export #:node #:node-inside #:node-outside
           #:level #:make-level
           #:level-map #:level-rotation-map
           #:level-steps #:level-max-steps #:level-state

           #:make-level-map #:add-hexagon
           #:hexagon-rotate #:key-rotate

           #:with-map #:with-new-map
           #:deftrip #:deftrash #:defbin #:defhex
           #:defrotate

           #:level-step)
  (:nicknames #:rht/core))

(defpackage #:rhombihexadeltille/levels
  (:use #:cl #:rht/core)
  (:export #:level)
  (:nicknames #:rht/levels))

(defpackage #:rhombihexadeltille/draw
  (:use #:cl #:rht/core #:rht/levels)
  (:nicknames #:rht/draw)
  (:import-from #:sketch
                #:defsketch #:translate #:with-pen #:make-pen #:with-font #:make-font
                #:ngon #:circle #:text
                #:+red+ #:+green+ #:+yellow+ #:+blue+ #:+black+ #:gray))
