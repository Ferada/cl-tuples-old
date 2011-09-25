
;; TO DO -- rewrite this to use assert instead of silly with-result macro

(asdf:oos 'asdf:load-op 'cl-tuples)

(defpackage :cl-tuples-test
  (:use :cl-tuples :cl)
  (:export "run-cl-tuples-tests"))

(in-package :cl-tuples-test)

(defmacro with-test (test-sym test &rest forms)
  (cl-tuples::with-gensyms (result)
	`(progn
	   ,@forms
	   (let
		   ((,result ,test))
		 (assert ,result)
		 (setf ,test-sym (and ,test-sym ,result))))))

(defparameter *result* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-tuple-symbol 'quad 'fixnum 0 '(a b  c d)))

(cl-tuples::def-tuple quad)

(quad-values* 8 4 3 1)

(cl-tuples::def-tuple-struct quad)

(cl-tuples::def-tuple-maker quad)

(cl-tuples::def-tuple-setter quad)

(cl-tuples::def-tuple-getter quad)

(defparameter *quad* (make-quad 3 7 5 9))

(assert (equalp *quad* #(3 7 5 9)))

(assert (equalp (multiple-value-list (quad* *quad*)) '(3 7 5 9))) 

(cl-tuples::def-tuple-set quad)

(set-quad *quad* 5 1 2 3)

(assert (equalp *quad* #(5 1 2 3)))

(quad-setter* *quad* #{9 10 7 6})

(assert (equalp *quad* #(9 10 7 6)))

(cl-tuples::def-new-tuple quad)

(new-quad)

(assert (equalp (make-quad 5 6 10 11) #(5 6 10 11)))

(cl-tuples::def-tuple-maker* quad)

(assert (equalp (make-quad* #{ 5 2 9 12 }) #(5 2 9 12)))

(cl-tuples::def-tuple-array-maker quad)

(defparameter *quads* (make-quad-array 3 :initial-element 0 :adjustable t :fill-pointer 2))
;; here..

(cl-tuples::def-tuple-aref* quad)

(assert (equalp (quad-aref* *quads* 1) #{0 0 0 0}))

(cl-tuples::def-tuple-aref quad)

(assert (equalp (quad-aref *quads* 1) #(0 0 0 0)))

(quad-aref *quads* 1)

(cl-tuples::def-tuple-aref-setter* quad)

(quad-aref-setter* *quads* 1 #{ 4 5 6 19 })

(assert (equalp (quad-aref *quads* 1) #(4 5 6 19)))

;; array dimensions
(cl-tuples::def-tuple-array-dimensions quad)

(assert (= (quad-array-dimensions *quads*) 2))

;; array extension
(setf *quads* (make-quad-array 3 :initial-element 0 :adjustable t :fill-pointer 2))

(cl-tuples::def-tuple-vector-push quad)

(quad-vector-push  #( 8 9 22 34 ) *quads*)

(assert (equalp (quad-aref *quads* 2) #(8 9 22 34)))

(cl-tuples::def-tuple-vector-push-extend quad)

(quad-vector-push-extend   #( 27 28 29 34 ) *quads*)

(assert (equalp (quad-aref *quads* 3) #(27 28 29 34)))

;; array extension (values)
(setf *quads* (make-quad-array 3 :initial-element 0 :adjustable t :fill-pointer 2))

(cl-tuples::def-tuple-vector-push* quad)

(quad-vector-push*  #{ 8 9 22 34 } *quads*)

(assert (equalp (quad-aref *quads* 2) #(8 9 22 34)))

(cl-tuples::def-tuple-vector-push-extend* quad)

(quad-vector-push-extend*   #{ 27 28 29 34 } *quads*)

(assert (equalp (quad-aref *quads* 3) #(27 28 29 34)))

(cl-tuples::def-with-tuple quad)

(with-quad *quad* (e1 e2 e3 e4) (assert (equalp (list e1 e2 e3 e4) '(9 10 7 6))))

(cl-tuples::def-with-tuple* quad)

(with-quad* #{ 5 6 7 9 } (e1 e2 e3 e4) (assert (equalp (list e1 e2 e3 e4) '(5 6 7 9))))

(cl-tuples::def-with-tuple-aref quad)

(with-quad-aref (*quads* 1 (el1 el2 el3 el4)) (assert (equalp (vector el1 el2 el3 el4) (quad-aref *quads* 1))))

;; generalised reference ?

(cl-tuples::def-tuple-setf*  quad)

(setf (quad* *quad*)  #{ -6 -6 -6 5})

(assert (equalp *quad* #(-6 -6 -6 5)))

(cl-tuples::def-tuple-array-setf*  quad)

(setf (quad-aref* *quads* 1) #{ -1 -2 -3 -5})

(assert (equalp (quad-aref *quads* 1) #(-1 -2 -3 -5)))

(cl-tuples::def-tuple-aref-setter quad)

(cl-tuples::def-tuple-array-setf quad)

(setf (quad-aref *quads* 3)  #( -3 -3 -7 -9))

(assert (equalp (quad-aref *quads* 3)  #( -3 -3 -7 -9)))

(def-tuple-type pair
	:tuple-element-type (unsigned-byte 8)
	:initial-element 0
	:elements (a b))

;; ;; basic operations
(with-test *result*
  (and (equalp *test-pair* #(3 4))
	   (equalp *pair-array* #(3 4)))
  (defparameter *test-pair* (make-pair 1 2))
  (defparameter *pair-array* (make-pair-array 2 :initial-element 0 :adjustable t :fill-pointer 1))
  (setf (pair* *test-pair*) (pair-values*  3 4))
  (setf (pair-aref* *pair-array* 0) (pair* *test-pair*)))


;; ;; array extension
(with-test *result*
  (equalp *pair-array* #(3 4 3 4 6 7))
  (pair-vector-push (pair *test-pair*) *pair-array*)
  (pair-vector-push-extend (pair*  6 7) *pair-array*))

;; ;; test with- forms
;; (with-test *result*
;;   (equalp *result* '(3 4))
;;   (setf *result*
;; 		(with-pair (pair *test-pair*) (a b)
;; 				   (list a b))))

;; (with-test *result*
;;   (equalp *result* '((3 4) (3 4) (6 7)))
;;   (setf *result*
;; 		(loop
;; 		   for i from 0 below (pair-array-dimensions *pair-array*)
;; 		   collect
;; 			 (with-pair-aref (*pair-array* i (a b))  (list a b)))))

;; ;; basic vector math
;; (defparameter *vector0* (make-vector3d #{ 0.0 0.0 0.0 } ))
;; (defparameter *vector1* (make-vector3d #{ 1.0 1.0 1.0 } ))
;; (defparameter *vectorx* (make-vector3d #{ 1.0 0.0 0.0 } ))
;; (defparameter *vectory* (make-vector3d #{ 0.0 1.0 0.0 } ))
;; (defparameter *vectorz* (make-vector3d #{ 0.0 0.0 1.0 } ))

;; (defparameter *test-vector* (new-vector3d))

;; (with-test *result*
;;   (= *result* 0.0)
;;   (setf *result* (vector3d-length (vector3d *vector0*))))

;; (with-test *result*
;;   (= *result* (sqrt 3.0))
;;   (setf *result* (vector3d-length (vector3d *vector1*))))

;; (with-test *result*
;;   (equalp *test-vector* #(0.57735026 0.57735026 0.57735026))
;;   (setf  *test-vector*
;; 		 (make-vector3d (vector3d-normal (vector3d *vector1*)))))

;; (with-test *result*
;;   (equalp *test-vector* #(0.0 0.0 1.0))
;;   (setf (vector3d *test-vector*)
;; 		(vector3d-cross (vector3d *vectorx*) (vector3d *vectory*))))

;; (with-test *result*
;;   (= *result* 0.57735026)
;;   (setf  *result* (vector3d-dot
;; 				   (vector3d *vectorx*) (vector3d-normal  (vector3d *vector1*)))))

;; ;; test identity mult

;; (defparameter *test-matrix* (make-matrix44 (cl-tuples::make-test-matrix44)))
;; (defparameter *identity-matrix* (make-matrix44 (identity-matrix44)))

;; (defparameter *vertex0* (make-vertex3d (vector3d-vertex3d (vector3d  *vector0*))))
;; (defparameter *vertex1* (make-vertex3d (vector3d-vertex3d (vector3d  *vector1*))))
;; (defparameter *vertexx* (make-vertex3d #{1.0 0.0 0.0 1.0}))
;; (defparameter *vertexy* (make-vertex3d #{0.0 1.0 0.0 1.0}))
;; (defparameter *vertexz* (make-vertex3d #{0.0 0.0 1.0 0.0}))

;; (with-test *result*
;;   (equalp *test-vector* #(1.0 1.0 1.0))
;;   (setf *test-vector* (make-vector3d (delta-vector3d  (vertex3d  *vertex0*)  (vertex3d *vertex1*)))))

;; (with-test *result*
;;   (= *result* 1.7320508)
;;   (setf *result*
;; 		(vertex3d-distance (vertex3d  *vertex0*) (vertex3d  *vertex1*))))


;; (defun torad (x) (coerce  (* x (/ PI 180.0)) 'single-float))

;; ;; basic matrix math
;; (defparameter *rotatex* (make-matrix44 (rotatex-matrix44 (torad 90))))
;; (defparameter *rotatey* (make-matrix44 (rotatey-matrix44 (torad 90))))
;; (defparameter *rotatez* (make-matrix44 (rotatez-matrix44 (torad 90))))

;; (defparameter *vertexx0* (make-vertex3d  (transform-vertex3d
;; 										  (matrix44  *rotatex*)
;; 										  (vertex3d  *vertexx*))))

;; (defparameter *vertexx1* (make-vertex3d
;; 						  (transform-vertex3d
;; 						   (matrix44  *rotatey*)
;; 						   (vertex3d  *vertexx0*))))

;; (defparameter *vertexx2* (make-vertex3d
;; 						  (transform-vertex3d
;; 						   (matrix44  *rotatez*)
;; 						   (vertex3d  *vertexx1*))))

;; (defparameter *concat-transform*
;;   (make-matrix44 (matrix44-product
;; 				  (matrix44 *rotatex*)
;; 				  (matrix44-product (matrix44  *rotatey*) (matrix44 *rotatez*)))))

;; (defparameter *vertexx3* (make-vertex3d
;; 						  (transform-vertex3d
;; 						   (matrix44 *concat-transform*)
;; 						   (vertex3d *vertexx0*))))


;; (defparameter *vector-array* (make-vector3d-array 2 :adjustable t :fill-pointer 1))

;; (setf (vector3d-aref *vector-array* 0) (vector3d *vectorx*))

;; ;; to do - should return size
;; (vector3d-vector-push  (vector3d  *vectory*) *vector-array*)

;; ;; to do - doesnt extend array properly
;; (vector3d-vector-push-extend (vector3d *vectorz*) *vector-array*)

;; ;; ;; iterate across array, apply transforms
;; (loop
;;    for i from 0 below (vector3d-array-dimensions *vector-array*)
;;    do
;; 	 (setf (vector3d-aref *vector-array* i)
;; 		   (cl-tuples::transform-vector3d
;; 			(matrix44 *concat-transform*)
;; 			(vector3d-aref *vector-array* i))))



;; ;; quick test case for clos wrapper

;; (def-tuple-class camera
;; 	(:tuples
;; 	 ((up :type cl-tuples::vector3d)
;; 	  (forward :type vector3d)
;; 	  (location :type vertex3d)
;; 	  (vertices :type vertex3d :array 5))
;; 	 :slots
;; 	 ((focal-length :type single-float :accessor focal-length-of)
;; 	  (id :allocation :class :reader id-of))))

;; ;;*test stanza*
;; (defparameter *test-camera* (make-instance 'camera))
;; (setf (up-of *test-camera*) #{ 0.0 0.0 0.0 })
;; (up-of *test-camera*)
;; (setf (up-of *test-camera*) #{ 1.0 2.0 3.0 })
;; (setf (vertices-of *test-camera* 3) #{ 2.0 3.0 -2.5 1.0 })
;; (vertices-of *test-camera* 3)
;; (vertices-of *test-camera* 4)
;; (vertices-of *test-camera* 1)

;; (defparameter *test-shape* (make-vector3d-array 4))

;; (setf (vector3d-aref *test-shape* 0) (vector3d* 3.14 0.0 3.14))
;; (setf (vector3d-aref *test-shape* 1) (vector3d* 3.14 0.0 -3.14))
;; (setf (vector3d-aref *test-shape* 2) (vector3d* -3.14 0.0 -3.14))
;; (setf (vector3d-aref *test-shape* 3) (vector3d* -3.14 0.0 3.14))


;; (defparameter *test-quaternion* (make-quaternion
;; 								 (angle-axis-quaternion
;; 								  (angle-axis* 0.0 1.0 0.0 (/ 3.14 2.0)))))


;; (defparameter *test-matrix*
;;   (make-matrix33
;;    (quaternion-matrix33 (quaternion *test-quaternion*))))


;; (loop
;;    for index from 0 below (vector3d-array-dimensions *test-shape*)
;;    do
;; 	 (setf (vector3d-aref *test-shape* index)
;; 		   (quaternion-transform-vector3d
;; 			(vector3d-aref *test-shape* index)
;; 			(quaternion *test-quaternion*))))

;; (loop
;;    for index from 0 below (vector3d-array-dimensions *test-shape*)
;;    do
;; 	 (setf (vector3d-aref *test-shape* index)
;; 		   (transform-vector3d
;; 			(matrix44-matrix33 (matrix44 *rotatey*))
;; 			(vector3d-aref *test-shape* index))))
