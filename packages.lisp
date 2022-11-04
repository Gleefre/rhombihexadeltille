(defpackage #:rhombihexadeltille/geometry
  (:use #:cl)
  (:export #:+all-directions+ #:+directions+ #:+t-directions+ #:+sq-directions+
           #:hex-> #:integer-neighbours
           #:hex-to-xy #:xy-to-hex)
  (:nicknames #:rht/geometry))

(defpackage #:rhombihexadeltille/core
  (:use #:cl #:rht/geometry)
  (:export #:node #:node-inside #:node-outside #:make-node
           #:make-level-map #:add-hexagon
           #:hexagon-rotate #:key-rotate

           #:level #:make-level
           #:level-map #:level-rotation-map
           #:level-steps #:level-max-steps #:level-state

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
  (:use #:cl #:sketch
        #:rht/core #:rht/levels #:rht/geometry)
  (:nicknames #:rht/draw))
