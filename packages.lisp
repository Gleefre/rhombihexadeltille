(defpackage #:rhombihexadeltille/geometry
  (:use #:cl)
  (:export #:+all-directions+ #:+directions+ #:+t-directions+ #:+sq-directions+
           #:hex-> #:integer-neighbours
           #:hex-to-xy #:xy-to-hex)
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

(defpackage #:rhombihexadeltille/sketch
  (:use #:cl #:sketch
        #:rht/core #:rht/levels #:rht/geometry)
  (:export #:start)
  (:nicknames #:rht/draw))

(defpackage #:rhombihexadeltille
  (:use #:cl #:rht/draw)
  (:export #:start)
  (:nicknames #:rht))
