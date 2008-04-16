
(in-package :cl-user)

(defpackage :cl-tuples
  (:use :cl)
  (:export make-tuple-symbol

           def-tuple-type
           def-tuple-op

           vector2d-dot
           vector2d-mag-square
           vector2d-length
           vector2d-normal
           vector2d-vertex2d

           vertex2d-vector2d

           vector3d-dot
           vector3d-mag-square
           vector3d-length
           vector3d-normal
           vector3d-cross
           vector3d-vertex3d

           vertex3d-vector3d
           vertex3d-distance
           delta-vector3d 

           identity-matrix44
           translation-matrix44
           rotatex-matrix44
           rotatey-matrix44
           rotatez-matrix44

           transform-vertex2d
           transform-vertex3d
           transform-vertex2d
           transform-vertex3d
           matrix33-product
           matrix44-product

           quaternion-conjugate
           quaternion-dot
           quaternion-mag-square
           quaternion-mag
           quaternion-inverse
           quaternion-product
           quaternion-matrix33
           angle-axis-quaternion
           quaternion-transform-vector3d)

           (:nicknames :tuples))

(in-package :cl-tuples)