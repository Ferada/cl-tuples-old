
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
           (vector2d-values* (* s x) (* s y))))

(def-tuple-op vector2d-length* 
    ((vec vector2d (x y)))
  (:return fast-float
           (sqrt (+ (* x x) (* y y)))))

(def-tuple-op vector2d-dot*
    ((veca vector2d (xa ya))
     (vecb vector2d (xb yb)))
  (:return fast-float
           (sqrt (+ (* xa xb) (* ya yb)))))


(def-tuple-op vector2d-normal* 
    ((vec vector2d (x y)))
  (:return vector2d
           (let ((mag (/ 1.0 (vector2d-length vec))))
             (vector2d-values*
              (* x mag)
              (* y mag)))))

(def-tuple-op vector2d-vertex2d*
    ((vec vector2d (x y)))
  (:return vertex3d
           (vertex3d-values* x y 1.0)))

(def-tuple-op vertex2d-vector2d*
    ((vert vertex2d (x y w)))
  (:return vector2d
           (vector2d-values* x y)))


(def-tuple-op vector3d-scale* 
    ((vec vector3d (x y z))
     (s fast-float))
  (:return vector3d
           (vector3d-values* (* s x) (* s y) (* s z))))
  
(def-tuple-op vector3d-length*
    ((vec vector3d (x y z)))
  (:return fast-float
           (sqrt (+ (* x x) (* y y) (* z z)))))

(def-tuple-op vector3d-dot*
    ((veca vector3d (xa ya za))
     (vecb vector3d (xb yb zb)))
  (:return fast-float
           (+ (* xa xb) (* ya yb) (* za zb))))

(def-tuple-op vector3d-difference*
    ((veca vector3d (xa ya za))
     (vecb vector3d (xb yb zb)))
     (:return vector3d
              (vector3d-values*
               (- xa xb)
               (- ya yb)
               (- za zb))))

(def-tuple-op vector3d-sum*
    ((veca vector3d (xa ya za))
     (vecb vector3d (xb yb zb)))
     (:return vector3d
              (vector3d-values*
               (+ xa xb)
               (+ ya yb)
               (+ za zb))))

(def-tuple-op vector3d-normal* 
    ((vec vector3d (x y z)))
  (:return vector3d
           (let 
               ((mag (vector3d-length vec)))
             (vector3d-values*
              (/ x mag)
              (/ y mag)
              (/ z mag)))))


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
    ((start vertex3d (ox oy oz ow))
     (end vertex3d (ex ey ez ew)))
  (:return fast-float
           (vector3d-length (vector3d-values* (- ex ox) (- ey oy) (- ez oz)))))

(def-tuple-op delta-vector3d*
    ((start vertex3d (ox oy oz ow))
     (end vertex3d (ex ey ez ew)))
  (:return vector3d
           (vector3d-values*  (- ex ox) (- ey oy) (- ez oz))))

;; TO DO 

;; convert 2 3d vectors to angle axis

;; construct 44 matrix from 3 / a2 3d vectors

