
(in-package :cl-tuples)


(def-tuple-type vector2d 
    :tuple-element-type single-float 
    :elements (x y))

(def-tuple-type vertex2d 
    :tuple-element-type single-float 
    :elements (x y w))

(def-tuple-type vector3d 
    :tuple-element-type single-float 
    :elements (x y z))

(def-tuple-type vertex3d 
    :tuple-element-type single-float
    :elements (x y z w))

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
     (with-vector-2d
         ,vector2d
       (x y)
       (values (/ x length) (/ y length)))))

(defmacro vector2d->vertex2d (vector2d)
  `(with-vector2d ,vector2d
     (x y)
     (values x y 1)))

(defmacro vertex2d->vector2d (vertex2d)
  `(with-vertex2d ,vertex2d
     (x y w)
     (values x y)))

;; make 33 matrix from 2 2d vectors


(defmacro vector3d-mag-square (vector3d)
  `(reduce-vector3d-tuple 
    #'+
    (map-vector3d-tuples #'* ,vector3d ,vector3d)))

(defmacro vector3d-length (vector3d)
  `(sqrt (vector3d-mag-square ,vector3d)))

;; (def-tuple-op vector3d-dot
;;   ((vector3d-lhs vector3d (x y z))
;;    (vector3d-rhs vector3d (n o p)))
;;   (format t "Hello"))

(defmacro vector3d-dot (vector3d-lhs vector3d-rhs)
  `(reduce-vector3d-tuple 
    #'+ (map-vector3d-tuples #'* ,vector3d-lhs ,vector3d-rhs)))

(defmacro vector3d-difference (vector3d-lhs vector3d-rhs)
  `(map-vector3d-tuples  #'- ,vector3d-lhs ,vector3d-rhs))

(defmacro vector3d-sum (vector3d-lhs vector3d-rhs)
  `(map-vector3d-tuples  #'+ ,vector3d-lhs ,vector3d-rhs))

(defmacro vector3d-normal (vector3d)
  `(let 
       ((mag (vector3d-length ,vector3d)))
     (with-vector-3d
         ,vector3d
       (x y z)
       (values (/ x length) (/ y length) (/ z length)))))

(defmacro vector3d->vertex3d (vector3d)
  `(with-vector3d ,vector3d 
     (x y z)
    (values x y w 1)))

(defmacro vertex3d->vector3d (vertex3d)
  `(with-vertex3d ,vertex3d
     (x y z w)
     (values x y z)))


(defmacro vector3d-cross (vector3d-lhs vector3d-rhs)
  `(with-vector3d 
      ,vector3d-lhs
     (lhs-x lhs-y lhs-z)
     (with-vector-3d 
         ,vector3d-rhs
       (rhs-x rhs-y rhs-z)
       (values (- (* lhs-y rhs-z) (* lhs-z rhs-y))
               (- (* lhs-z rhs-x) (* lhs-x rhs-z))
               (- (* lhs-z rhs-y) (* lhs-a rhs-x))))))

(def-tuple-op vertex3d-distance
    ((start vertex3d (ox oy oz ow))
     (end vertex3d (ex ey ez ew)))
  (vector3d-length (values (- ex ox) (- ey oy) (- ez oz))))

(def-tuple-op delta-vector3d
    ((start vertex3d (ox oy oz ow))
     (end vertex3d (ex ey ez ew)))
  (vector3d-tuple  (- ex ox) (- ey oy) (- ez oz)))

;; convert 2 3d vectors to angle axis

;; construct 44 matrix from 3 / a2 3d vectors



