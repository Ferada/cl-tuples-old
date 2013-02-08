(in-package :cl-tuples)

;; type defs

;; vectors

(def-tuple-type vector2d
  :tuple-element-type fast-float
  :initial-element 0.0f0
  :elements (x y))

(export-tuple-operations vector2d)

(def-tuple-type vector3d
  :tuple-element-type fast-float
  :initial-element 0.0f0
  :elements (x y z))

(export-tuple-operations vector3d)

;; vertices
(def-tuple-type vertex2d
    :tuple-element-type fast-float
    :elements (x y w))

(export-tuple-operations vertex3d)

(def-tuple-type vertex3d
    :tuple-element-type fast-float
    :elements (x y z w))

(export-tuple-operations vertex3d)

;; primitives

(def-tuple-op vector2d-scale*
    ((vec vector2d (x y))
     (s fast-float))
  (:return vector2d
           (vector2d-map* (*)
            vec
            (vector2d-key-values :initial-element s))))

(def-tuple-op vector2d-dot*
    ((veca vector2d)
     (vecb vector2d))
  (:return fast-float
           (vector2d-reduce*
            (+) (vector2d-map* (*) veca vecb))))

(def-tuple-op vector2d-length-square*
    ((vec vector2d (x y)))
  (:return fast-float
           (vector2d-dot* vec vec)))

(def-tuple-op vector2d-length*
    ((vec vector2d (x y)))
  (:return fast-float
           (sqrt (vector2d-length-square* vec))))

(def-tuple-op vector2d-normal*
    ((vec vector2d (x y)))
  (:return vector2d
           (vector2d-scale*
            vec
            (/ (vector2d-length* vec)))))

(def-tuple-op vector2d-vertex2d*
    ((vec vector2d (x y)))
  (:return vertex3d
           (vertex3d-values* x y 1.0)))

(def-tuple-op vertex2d-vector2d*
    ((vert vertex2d (x y w)))
  (:return vector2d
           (vector2d-values* x y)))

(def-tuple-op vector3d-scale*
    ((vec vector3d)
     (s fast-float))
  (:return vector3d
           (vector3d-map* (*)
            (vector3d-key-values :initial-element s)
            vec)))

(def-tuple-op vector3d-dot*
    ((veca vector3d (xa ya za))
     (vecb vector3d (xb yb zb)))
  (:return fast-float
           (vector3d-reduce*
            (+) (vector3d-map* (*) veca vecb))))

(def-tuple-op vector3d-length*
    ((vec vector3d))
  (:return fast-float
           (sqrt (vector3d-dot* vec vec))))

(def-tuple-op vector3d-difference*
    ((veca vector3d)
     (vecb vector3d))
     (:return vector3d
              (vector3d-map* (-) veca vecb)))

(def-tuple-op vector3d-sum*
    ((veca vector3d)
     (vecb vector3d))
  (:return vector3d
           (vector3d-map* (+) veca vecb)))

(def-tuple-op vector3d-normal*
    ((vec vector3d))
  (:return vector3d
           (vector3d-scale*
            vec
            (/ (vector3d-length* vec)))))

(def-tuple-op vector3d-vertex3d*
    ((vec vector3d (x y z)))
  (:return vertex3d
           (vertex3d-values* x y z 1.0)))

(def-tuple-op vertex3d-vector3d*
    ((vert vertex3d (x y z w)))
  (:return vector3d
           (vector3d-values* x y z)))

(def-tuple-op vector3d-cross*
    ((lhs vector3d (lhs-x lhs-y lhs-z))
     (rhs vector3d (rhs-x rhs-y rhs-z)))
  (:return vector3d
           (vector3d-values*
            (- (* lhs-y rhs-z) (* lhs-z rhs-y))
            (- (* lhs-z rhs-x) (* lhs-x rhs-z))
            (- (* lhs-x rhs-y) (* lhs-y rhs-x)))))

(def-tuple-op vertex3d-distance*
    ((start vertex3d)
     (end vertex3d))
  (:return fast-float
           (vector3d-length*
            (vector3d-difference*
             (vertex3d-vector3d* end)
             (vertex3d-vector3d* start)))))

(def-tuple-op delta-vector3d*
    ((start vertex3d)
     (end vertex3d))
  (:return vector3d
           (vector3d-difference* end start)))

;; TO DO

;; convert 2 3d vectors to angle axis

;; construct 44 matrix from 3 / a2 3d vectors
