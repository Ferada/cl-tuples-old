
(defpackage :cl-tuples-test
  (:use :cl-tuples :cl)
  (:export "run-cl-tuples-tests"))

(in-package :cl-tuples-test)



;; (cl-tuples::def-tuple pair)
;; (cl-tuples::def-tuple-array-dimensions pair)
;; (cl-tuples::def-tuple-getter pair)
;; (cl-tuples::def-tuple-aref pair)
;; (cl-tuples::def-with-tuple pair)
;; (cl-tuples::def-with-tuple* pair)
;; (cl-tuples::def-with-tuple-aref pair)
;; (cl-tuples::def-tuple-setter  pair)
;; (cl-tuples::def-tuple-aref-setter  pair)
;; (cl-tuples::def-tuple-vector-push pair)
;; (cl-tuples::def-tuple-vector-push-extend pair)
;; (cl-tuples::def-new-tuple pair)
;; (cl-tuples::def-tuple-maker pair)
;; (cl-tuples::def-tuple-maker* pair)
;; (cl-tuples::def-tuple-array-maker pair)
;; (cl-tuples::def-tuple-setf  pair)
;; (cl-tuples::def-tuple-array-setf  pair)
;; (cl-tuples::def-tuple-map pair)
;; (cl-tuples::def-tuple-reduce pair)



(defmacro with-test (test-sym test &rest forms)
  (cl-tuples::with-gensyms (result)
    `(progn
       ,@forms
       (let 
           ((,result ,test))
         (assert ,result)
         (setf ,test-sym (and ,test-sym ,result))))))

(defparameter *result* t)

(def-tuple-type pair
    :tuple-element-type (unsigned-byte 8)
    :elements (a b))

;; basic operations
(with-test *result*
  (and (equalp *test-pair* #(3 4))
       (equalp *pair-array* #( 3 4)))
  (defparameter *test-pair* (make-pair (pair-tuple 1 2)))
  (defparameter *pair-array* (make-pair-array 2 :adjustable t :fill-pointer 1))
  (setf *test-pair* (make-pair #{ 3 4 }))
  (setf (pair-aref *pair-array* 0) (pair *test-pair*)))


;; array extension
(with-test *result*
  (equalp *pair-array* #(3 4 3 4 6 7))
  (pair-vector-push (pair *test-pair*) *pair-array*)
  (pair-vector-push-extend (pair-tuple  6 7) *pair-array*))

;; map/reduce
(with-test *result* 
  (and (equalp *test-pair* #(7 10))
       (= *result* 4))
  (setf *test-pair* (make-pair (map-pair-tuples #'+ #{ 1 2 } #{ 2 3 } #{ 4 5 })))
  (setf *result* (reduce-pair-tuple #'* #{ 2 2 })))

;; basic vector math
(defparameter *vector0* (make-vector3d #{ 0.0 0.0 0.0 } )) 
(defparameter *vector1* (make-vector3d #{ 1.0 1.0 1.0 } ))
(defparameter *vectorx* (make-vector3d #{ 1.0 0.0 0.0 } ))
(defparameter *vectory* (make-vector3d #{ 0.0 1.0 0.0 } ))
(defparameter *vectorz* (make-vector3d #{ 0.0 0.0 1.0 } ))

(defparameter *test-vector* (new-vector3d))

(map-vector3d-tuples #'+ (vector3d  *vector0*) (vector3d *vector1*))
(reduce-vector3d-tuple #'- (vector3d *vector1*))

(with-test *result*
  (= *result* 0.0)
  (setf *result* (vector3d-length (vector3d *vector0*))))

(with-test *result*
  (= *result* (sqrt 3.0))
  (setf *result* (vector3d-length (vector3d *vector1*))))

(with-test *result*
 (equalp *test-vector* #(0.57735026 0.57735026 0.57735026))
 (setf  *test-vector*
       (make-vector3d (vector3d-normal (vector3d *vector1*)))))

(with-test *result*
  (equalp *test-vector* #(0.0 0.0 1.0))
  (setf (vector3d *test-vector*)
        (vector3d-cross (vector3d *vectorx*) (vector3d *vectory*))))

(with-test *result*
  (= *result* 0.57735026)
  (setf  *result* (vector3d-dot 
                  (vector3d *vectorx*) (vector3d-normal  (vector3d *vector1*)))))

(defparameter *vertex0* (make-vertex3d (vector3d-vertex3d (vector3d  *vector0*))))
(defparameter *vertex1* (make-vertex3d (vector3d-vertex3d (vector3d  *vector1*))))
(defparameter *vertexx* (make-vertex3d (vector3d-vertex3d (vector3d  *vectorx*))))
(defparameter *vertexy* (make-vertex3d (vector3d-vertex3d (vector3d  *vectory*))))
(defparameter *vertexz* (make-vertex3d (vector3d-vertex3d (vector3d  *vectorz*))))

(with-test *result*
  (equalp *test-vector* #(1.0 1.0 1.0))
  (setf *test-vector* (make-vector3d (delta-vector3d (vector3d  *vertex0*) (vector3d *vertex1*)))))

(with-test *result*
  (= *result* 1.7320508)
  (setf *result*
        (vertex3d-distance (vector3d  *vertex0*) (vector3d  *vertex1*))))

;; basic matrix math
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
  


