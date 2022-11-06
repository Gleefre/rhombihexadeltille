(in-package #:rht/core)

;; Node is a single ngon
;; It can contain a mover inside and a goal/actor outside

;; A mover is one of nil, :trash or keyword for color
;; Possible colors: (:red :yellow :green :blue)
;; To add colors you need to add it to *color-map* variable in rht/draw package (alist)

;; A goal if (:color color) where color is one of possible colors.
;; An actor is one of (:bin safe?) or (:rotation key direction rotated)
;; safe? is NIL or T, depending on is it safe for movers to stay there
;; direction is one of :clock or :counter-clock
;; rotated is number of times it was rotated
;; Possible keys: (:a :s :w :d)
;; To add keys you need to add it to *key-map* variable in rht/draw package (alist)

(defstruct node
  outside inside)

;; Functions to manipulate map of nodes

(defun add-hexagon (map hx hy)
  (loop for dir in +all-directions+
        for point = (hex-> dir hx hy)
        do (setf (gethash point map)
                 (gethash point map (make-node)))))

(defmacro rotate-insides (list-name length start k)
  `(rotatef ,@(loop for i = start then (+ i k)
                    repeat (/ length (gcd length (abs k)))
                    collect `(node-inside (nth ,(mod i length) ,list-name)))))

(defun hexagon-rotate (map x y direction)
  (let ((triangles (loop for dir in +t-directions+
                         collect (gethash (hex-> dir x y) map)))
        (squares   (loop for dir in +sq-directions+
                         collect (gethash (hex-> dir x y) map))))
    (incf (fourth (node-outside (gethash (list x y) map))))
    (if (eql direction :clock)
        (progn (rotate-insides squares   6 0 -1)
               (rotate-insides triangles 6 0 -1))
        (progn (rotate-insides squares   6 0  1)
               (rotate-insides triangles 6 0  1)))))

;; Level consists of map, rotation map, number of steps done, steps restriction and win/lose/play state

(defstruct level
  (hexagon-map (make-hash-table :test 'equal))
  (rotation-map (make-hash-table))
  (steps 0)
  (max-steps 0) ; 0 means no restriction
  (state :play) ; :won / :lost / :play
  (text "")) ; for tutorial levels

;; Helpers for parsing coordinates passed to defl/ macros

(defun single-coord? (coords)
  (or (not (listp coords))
      (numberp (first coords))
      (keywordp (first coords))))

(defun parse-coord (coord)
  (cond
    ((not (listp coord)) coord)
    ((numberp (first coord)) `(list ,@coord))
    ((keywordp (first coord)) `(hex-> ,@coord))
    (t coord)))

(defun parse-coords (coords)
  (cond
    ((not (listp coords)) coords)
    ((single-coord? coords) `(list ,(parse-coord coords)))
    (t `(list ,@(loop for c in coords
                      collect (parse-coord c))))))

;; Macros to define level

(defmacro with-new-level (&body body)
  `(let ((level (make-level)))
     (with-slots (hexagon-map rotation-map max-steps text) level
       ,@body)
     level))

(defmacro with-level (level-form &body body)
  `(let ((level ,level-form))
     (with-slots (hexagon-map rotation-map max-steps text) level
       ,@body)
     level))

(defmacro defl/hex (coords)
  `(loop for c in ,(parse-coords coords)
         do (apply #'add-hexagon hexagon-map c)))

(defmacro defl/trip (color depart arrive)
  `(let ((color ,color))
     (loop for c in ,(parse-coords depart)
           do (setf (node-inside (gethash c hexagon-map))
                    color))
     (loop for c in ,(parse-coords arrive)
           do (setf (node-outside (gethash c hexagon-map))
                    (list :color color)))))

(defmacro defl/trash (coords)
  `(loop for c in ,(parse-coords coords)
         do (setf (node-inside (gethash c hexagon-map))
                  :trash)))

(defmacro defl/bin (coords safe?)
  `(let ((safe ,safe?))
     (loop for c in ,(parse-coords coords)
           do (setf (node-outside (gethash c hexagon-map))
                    (list :bin safe)))))

(defmacro defl/rotate (key direction coords)
  `(let ((key ,key)
         (direction ,direction))
     (loop for c in ,(parse-coords coords)
           do (setf (node-outside (gethash c hexagon-map))
                    (list :rotation key direction 0))
           do (push (list c direction)
                    (gethash key rotation-map nil)))))

(defmacro defl/maxsteps (n)
  `(setf max-steps ,n))

(defmacro defl/text (text)
  `(setf text ,text))

;; Perform a rotation by its name

(defun key-rotate (level key)
  (with-slots (hexagon-map rotation-map) level
    (loop for ((x y) direction) in (gethash key rotation-map)
          do (hexagon-rotate hexagon-map x y direction))))

;; Throw all to the bins
;; You lost if a mover is thrown to the bin

(defun throw-to-bins (level)
  (maphash (lambda (_ node)
             (declare (ignore _))
             (when (and (eql (car (node-outside node)) :bin)
                        (node-inside node))
               (case (node-inside node)
                 (:trash (setf (node-inside node) nil))
                 (t (unless (second (node-outside node)) (setf (level-state level) :lost))))))
           (level-hexagon-map level)))

;; Functions to check if the level was passed

(defun not-arrived? (node)
  (case (node-inside node)
    ((:red :green :yellow :blue) (not (eql (node-inside node) (cadr (node-outside node)))))
    (:trash t)))

(defun arrived? (node)
  (not (not-arrived? node)))

(defun all-arrived? (hexagon-map)
  (loop for node being the hash-values of hexagon-map
        never (not-arrived? node)))

(defun win? (level)
  (with-slots (hexagon-map steps max-steps) level
    (and (all-arrived? hexagon-map)
         (or (zerop max-steps)
             (<= steps max-steps)))))

;; Making a step
;; We need to (1) count a step, then (2) make a rotation,
;; then to (3) throw trash to the bins, then (4) update win/lose state

(defun level-step (level rotation-key)
  (with-slots (state steps max-steps rotation-map) level
    (when (and (eql state :play) ; step further only if it is playable
               (gethash rotation-key rotation-map)) ; and if rotation is defined
      (incf steps)
      (key-rotate level rotation-key)
      (throw-to-bins level)
      (cond ((win? level) (setf state :won))
            ((= steps max-steps) (setf state :lost))))))
