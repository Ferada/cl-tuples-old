
(defpackage :cl-tuples-test
  (:use :cl-tuples :cl)
  (:export "run-cl-tuples-tests"))

(in-package :cl-tuples-test)


(def-tuple-type pair
    :tuple-element-type (unsigned-byte 8)
    :elements (a b))

(defparameter *test-pair* (make-pair (pair-tuple 1 2)))
(defparameter *pair-array* (make-pair-array 2 :adjustable t :fill-pointer 1))

(setf *test-pair* (make-pair #{ 3 4 }))

(pair-aref *pair-array* 0)

(setf (aref *pair-array* 0) (pair  *test-pair*))

(pair-vector-push (pair *test-pair*) *pair-array*)

(pair-vector-push-extend (pair-tuple 6 7) *pair-array*)

(map-pair-tuples #'+ #{ 1 2 } #{ 2 3 } #{ 4 5 })

(reduce-pair-tuple #'* #{ 2 2 })

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
                           (matrix44 *concat-transform*)
                           (vertex3d *vertexx0*))))


(defparameter *vector-array* (make-vector3d-array 2 :adjustable t :fill-pointer 1))

(setf (vector3d-aref *vector-array* 0) (vector3d *vectorx*))

;; to do - should return size
(vector3d-vector-push  (vector3d  *vectory*) *vector-array*)

;; to do - doesnt extend array properly
(vector3d-vector-push-extend (vector3d *vectorz*) *vector-array*)

;; iterate across array, apply transforms
(loop
   for i from 0 below (vector3d-array-dimensions *vector-array*)
   do (transform-vertex3d (vector3d-aref *vector-array* i)
                          (matrix44 *concat-transform*)))
  


