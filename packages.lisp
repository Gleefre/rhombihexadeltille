(defpackage #:rhombihexadeltille/geometry
  (:use #:cl)
  (:export #:+all-directions+ #:+directions+ #:+t-directions+ #:+sq-directions+
           #:hex-> #:integer-neighbours
           #:hex-to-xy #:xy-to-hex
           #:node-shape #:ngon-scale
           #:bounds)
  (:nicknames #:rht/geometry))

(defpackage #:rhombihexadeltille/core
  (:use #:cl #:rht/geometry)
  (:export #:node #:make-node #:node-inside #:node-outside
           #:add-hexagon #:hexagon-rotate #:key-rotate

           #:level #:make-level
           #:level-hexagon-map #:level-rotation-map
           #:level-steps #:level-max-steps #:level-state

           #:with-level #:with-new-level
           #:defl/hex #:defl/trip #:defl/trash
           #:defl/bin #:defl/rotate
           #:defl/maxsteps

           #:level-step)
  (:nicknames #:rht/core))

(defpackage #:rhombihexadeltille/levels
  (:use #:cl #:rht/core #:rht/geometry)
  (:export #:level)
  (:nicknames #:rht/levels))

(defpackage #:rhombihexadeltille/utils
  (:use #:cl)
  (:export #:do-later #:do-now)
  (:nicknames #:rht/utils))

(defpackage #:rhombihexadeltille/sketch/utils
  (:use #:cl #:sketch)
  (:export #:fit #:with-fit
           #:with-translate #:with-rotate #:with-scale)
  (:nicknames #:rht/sketch/utils))

(defpackage #:rhombihexadeltille/sketch
  (:use #:cl #:sketch
        #:rht/core #:rht/levels #:rht/geometry
        #:rht/utils #:rht/sketch/utils)
  (:export #:start)
  (:nicknames #:rht/sketch))

(defpackage #:rhombihexadeltille
  (:use #:cl #:rht/sketch)
  (:export #:start)
  (:nicknames #:rht))
