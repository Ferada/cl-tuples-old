
(in-package :cl-tuples)

(def-tuple-type plane
    :tuple-element-type single-float
    :elements (x y z d))

(defmacro plane->vector3d (plane)
  `(with-plane ,plane
     (x y z d)
     (values x y z)))

(defmacro plane->displacement (plane)
  `(with-plane ,plane
     (x y z d)
     d))

(defmacro vector3d->plane (vector displacement)
  `(with-vector3d ,vector
       (x y z)
     (values x y z ,displacement)))
