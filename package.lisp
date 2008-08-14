
(in-package :cl-user)

(defpackage :cl-tuples
  (:use :cl)
  (:export make-tuple-symbol

           tuple-typep
           tuple-size
           tuple-element-type
           tuple-elelents
           tuple-gensyms
           tuple-typespec
           tuple-typespec*
           tuple-typespec**
           
           def-tuple-type
           def-tuple-op

           vector2d-dot
           vector2d-mag-square
           vector2d-length
           vector2d-normal
           vector2d-vertex2d

           vertex2d-vector2d

           vector3d-dot
           vector3d-sum
           vector3d-difference
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
           transform-vector2d
           transform-vector3d
           matrix33-product
           matrix44-product
           matrix44-matrix33
           matrix33-marrix44

           quaternion-conjugate
           quaternion-dot
           quaternion-mag-square
           quaternion-mag
           quaternion-inverse
           quaternion-product
           quaternion-matrix33
           angle-axis-quaternion
           quaternion-transform-vector3d

           def-tuple-class

           width
           height)

  
           (:nicknames :tuples))

(in-package :cl-tuples)