
(in-package :cl-user)

(defpackage :cl-tuples
  (:use :cl :alexandria :iterate)
  (:nicknames :tuples)
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

           fast-float
           fast-pi

           vector2d-dot*
           vector2d-mag-square*
           vector2d-length*
           vector2d-normal*
           vector2d-vertex2d*
           vector2d-component-product*
           vector2d-scale*

           vertex2d-vector2d*

           vector3d-component-product*
           vector3d-scale*
           vector3d-dot*
           vector3d-sum*
           vector3d-difference*
           vector3d-mag-square*
           vector3d-length*
           vector3d-normal*
           vector3d-cross*
           vector3d-vertex3d*
           vertex3d-vector3d*
           vertex3d-distance*

           delta-vector3d*

           transpose-matrix33*
           identity-matrix44*
           translation-matrix44*
           scaling-matrix44*
           vertex3d-translation-matrix44*
           rotatex-matrix44*
           rotatey-matrix44*
           rotatez-matrix44*
           transpose-matrix44*

           matrix22-determinant*
           matrix33-determinant*
           matrix44-determinant*

           matrix22-scale*
           matrix33-scale*
           matrix44-scale*

           cofactor-matrix22*
           cofactor-matrix33*
           cofactor-matrix44*

           inverted-matrix22*
           inverted-matrix33*
           inverted-matrix44*

           transform-vertex2d*
           transform-vertex3d*
           transform-vector2d*
           transform-vector3d*
           matrix33-product*
           matrix44-product*
           matrix44-matrix33*
           matrix33-matrix44*

           quaternion-sum*
           quaternion-normalize*
           quaternion-scale*
           quaternion-conjugate*
           quaternion-dot*
           quaternion-mag-square*
           quaternion-mag*
           quaternion-inverse*
           quaternion-product*
           quaternion-matrix33*
           angle-axis-quaternion*
           quaternion-transform-vector3d*
           vector3d-quaternion*

           width
           height

           disable-tuples-syntax
           locally-disable-tuples-syntax
           enable-tuples-syntax
           locally-enable-tuples-syntax
           file-enable-tuples-syntax
           restore-tuples-syntax-state
           ))
