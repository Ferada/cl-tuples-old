
(in-package :cl-tuples)


(def-tuple-type vector2d 
    :tuple-element-type single-float 
    :elements (x y))

(export-tuple-operations vector2d)

(def-tuple-type vertex2d 
    :tuple-element-type single-float 
    :elements (x y w))

(export-tuple-operations vertex3d)

(def-tuple-type vector3d 
    :tuple-element-type single-float 
    :elements (x y z))

(export-tuple-operations vector3d)

(def-tuple-type vertex3d 
    :tuple-element-type single-float
    :elements (x y z w))

(export-tuple-operations vertex3d)


(defmacro vector2d-mag-square (vector2d)
  `(reduce-vector2d-tuple #'+ (map-vector2d-values #'* ,vector2d ,vector2d)))

(defmacro vector2d-length (vector2d)
  `(sqrt (vector2d-mag-square ,vector2d)))

(defmacro vector2d-dot (vector2d-lhs vector2d-rhs)
  `(reduce-vector2d-tuple 
    #'+ (map-vector2d-tuples #'* ,vector2d-lhs ,vector2d-rhs)))

(defmacro vector2d-normal (vector2d)
  `(let 
       ((mag (vector2d-length ,vector2d)))
     (with-vector2d
         ,vector2d
       (x y)
       (values (/ x length) (/ y length)))))

(defmacro vector2d-vertex2d (vector2d)
  `(with-vector2d ,vector2d
     (x y)
     (values x y 1)))

(defmacro vertex2d-vector2d (vertex2d)
  `(with-vertex2d ,vertex2d
     (x y w)
     (values x y)))

;; make 33 matrix from 2 2d vectors


(def-tuple-op vector3d-length
    ((vec vector3d (x y z)))
  (:return single-float
           (sqrt (+ (* x x) (* y y) (* z z)))))

(def-tuple-op vector3d-dot
    ((veca vector3d (xa ya za))
     (vecb vector3d (xb yb zb)))
  (:return single-float
           (+ (* xa xb) (* ya yb) (* za zb))))

(def-tuple-op vector3d-difference
    ((veca vector3d (xa ya za))
     (vecb vector3d (xb yb zb)))
     (:return vector3d
              (vector3d-tuple
               (- xa xb)
               (- ya yb)
               (- za zb))))

(def-tuple-op vector3d-sum
    ((veca vector3d (xa ya za))
     (vecb vector3d (xb yb zb)))
     (:return vector3d
              (vector3d-tuple
               (+ xa xb)
               (+ ya yb)
               (+ za zb))))

(def-tuple-op vector3d-normal 
    ((vec vector3d (x y z)))
  (:return vector3d
           (let 
               ((mag (sqrt (+ (* x x) (* y y) (* z z)))))
             (vector3d-tuple
              (/ x mag)
              (/ y mag)
              (/ z mag)))))


(def-tuple-op vector3d-vertex3d 
    ((vector3d vec (x y z)))
  (:return vertex3d
           (vertex3d-tuple x y z 1.0)))

(def-tuple-op vertex3d-vector3d 
    ((vert  vertex3d (x y z w)))
  (:return vector3d 
           (vector3d-tuple x y z)))
  

(def-tuple-op vector3d-cross
    ((lhs vector3d (lhs-x lhs-y lhs-z))
     (rhs vector3d (rhs-x rhs-y rhs-z)))
  (:return vector3d
           (vector3d-tuple
            (- (* lhs-y rhs-z) (* lhs-z rhs-y))
            (- (* lhs-z rhs-x) (* lhs-x rhs-z))
            (- (* lhs-x rhs-y) (* lhs-y rhs-x)))))


(def-tuple-op vertex3d-distance
    ((start vertex3d (ox oy oz ow))
     (end vertex3d (ex ey ez ew)))
  (:return single-float
           (vector3d-length (values (- ex ox) (- ey oy) (- ez oz)))))

(def-tuple-op delta-vector3d
    ((start vertex3d (ox oy oz ow))
     (end vertex3d (ex ey ez ew)))
  (:return vector3d
           (vector3d-tuple  (- ex ox) (- ey oy) (- ez oz))))

;; TO DO 

;; convert 2 3d vectors to angle axis

;; construct 44 matrix from 3 / a2 3d vectors

