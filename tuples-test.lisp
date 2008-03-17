
(defpackage :cl-tuples-test
  (:use :cl-tuples :cl))

(in-package :cl-tuples-test)

(defparameter *vector0* (make-vector3d #{ 0.0 0.0 0.0 } )) 
(defparameter *vector1* (make-vector3d #{ 1.0 1.0 1.0 } ))
(defparameter *vectorx* (make-vector3d #{ 1.0 0.0 0.0 } ))
(defparameter *vectory* (make-vector3d #{ 0.0 1.0 0.0 } ))
(defparameter *vectorz* (make-vector3d #{ 0.0 0.0 1.0 } ))

 
(map-vector3d-tuples #'+ (vector3d  *vector0*) (vector3d *vector1*))

(reduce-vector3d-tuple #'- (vector3d *vector1*))

(vector3d-length (vector3d *vector0*))

(vector3d-length (vector3d *vector1*))

(vector3d-normal (vector3d *vector1*))

(vector3d-cross (vector3d *vectorx*) (vector3d *vectory*))

(vector3d-dot (vector3d *vectorx*) (vector3d-normal  (vector3d *vector1*)))



(defparameter *vertex0* (make-vertex3d (vector3d-vertex3d (vector3d  *vector0*))))
(defparameter *vertex1* (make-vertex3d (vector3d-vertex3d (vector3d  *vector1*))))
(defparameter *vertexx* (make-vertex3d (vector3d-vertex3d (vector3d  *vectorx*))))
(defparameter *vertexy* (make-vertex3d (vector3d-vertex3d (vector3d  *vectory*))))
(defparameter *vertexz* (make-vertex3d (vector3d-vertex3d (vector3d  *vectorz*))))

(delta-vector3d (vector3d  *vertex0*) (vector3d *vertex1*))

(vertex3d-distance (vector3d  *vertex0*) (vector3d  *vertex1*))

(defparameter *rotatex* (make-matrix44 (rotatex-matrix44 45)))
(defparameter *rotatey* (make-matrix44 (rotatey-matrix44 45)))
(defparameter *rotatez* (make-matrix44 (rotatez-matrix44 45)))

(defparameter *vertexx0* (make-vertex3d  (transform-vertex3d 
                                         (matrix44  *rotatex*) 
                                         (vertex3d  *vertexx*))))

(defparameter *vertexx1* (make-vertex3d  
                          (transform-vertex3d 
                           (matrix44  *rotatey*) 
                           (vertex3d  *vertexx0*))))

(defparameter *vertexx2* (make-vertex3d  
                          (transform-vertex3d 
                           (matrix44  *rotatez*) 
                           (vertex3d  *vertexx1*))))

(defparameter *concat-transform* 
  (make-matrix44 (matrix44-product
                  (matrix44 *rotatex*) 
                  (matrix44-product (matrix44  *rotatey*) (matrix44 *rotatez*)))))

(defparameter *vertexx3* (make-vertex3d
                          (transform-vertex3d 
                           (vertex3d *vertexx0*)
                           (matrix44 *concat-transform*))))