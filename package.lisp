
(in-package :cl-user)

(defpackage :cl-tuples
  (:use :cl)
  (:export def-tuple-type
           def-tuple-op

           vector2d
           vector2d-tuple
           vector2d-array-dimensions
           new-vector2d
           make-vector2d
           make-vector2d*
           make-vector2d-array
           vector2d-aref
           with-vector2d
           with-vector2d-array
           map-vector2d-tuples
           reduce-vector2d-tuple

           vector2d-dot
           vector2d-mag-square
           vector2d-length
           vector2d-normal
           vector2d-vertex2d

           vertex2d
           vertex2d-tuple
           vertex2d-array-dimensions
           new-vertex2d
           make-vertex2d
           make-vertex2d-array
           vertex2d-aref
           with-vertex2d
           with-vertex2d-array
           map-vertex2d-tuples
           reduce-vertex2d-tuple
           vertex2d-vector2d

           vector3d
           vector3d-tuple
           vector3d-array-dimensions
           new-vector3d
           make-vector3d
           make-vector3d*
           make-vector3d-array
           vector3d-aref
           with-vector3d
           with-vector3d-array
           map-vector3d-tuples
           reduce-vector3d-tuple

           vector3d-dot
           vector3d-mag-square
           vector3d-length
           vector3d-normal
           vector3d-cross
           vector3d-vertex3d

           vertex3d
           vertex3d-tuple
           vertex3d-array-dimensions
           new-vertex3d
           make-vertex3d
           make-vertex3d*
           make-vertex3d-array
           vertex3d-aref
           with-vertex3d
           with-vertex3d-array
           map-vertex3d-tuples
           reduce-vertex3d-tuple
           vertex3d-vector3d
           vertex3d-distance
           delta-vector3d 

           matrix33
           matrix33-tuple
           matrix33-array-dimensions
           new-matrix33
           make-matrix33
           make-matrix33*
           make-matrix33-array
           matrix33-aref
           with-matrix33
           with-matrix33-array
           map-matrix33-tuples
           reduce-matrix33-tuple

           matrix44
           matrix44-tuple
           matrix44-array-dimensions
           new-matrix44
           make-matrix44
           make-matrix44*
           make-matrix44-array
           matrix44-aref
           with-matrix44
           with-matrix44-array
           map-matrix44-tuples
           reduce-matrix44-tuple
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

           quaternion
           quaternion-tuple
           quarternion-array-dimensions
           new-quaternion
           make-quaternion
           make-quaternion*
           make-quaternion-array
           quaternion-aref
           with-quaternion
           with-quaternion-array
           map-quaternion-tuples
           reduce-quaternion-tuple

           angle-axis
           angle-axis-tuple
           angle-axis-array-dimensions
           new-angle-axis
           make-angle-axis
           make-angle-axis*
           make-angle-axis-array
           angle-axis-aref
           with-angle-axis
           with-angle-axis-array
           map-angle-axis-tuples
           reduce-angle-axis-tuple

           quaternion-conjugate
           quaternion-dot
           quaternion-mag-square
           quaternion-mag
           quaternion-inverse
           quaternion-product
           quaternion-matrix33
           angle-axis-quaternion
           quaternion-transform-vector3d

           colour
           colour-tuple
           colour-array-dimensions
           new-colour
           make-colour
           make-colour*
           make-colour-array
           make-colour-aref
           with-colour
           with-colour-array
           map-colour-tuples
           reduce-colour-tuple)

           (:nicknames :tuples))

(in-package :cl-tuples)