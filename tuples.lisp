;;;; tuples.lisp

(in-package :cl-tuples)


(defun def-tuple (type-name)
  (tuple-expansion-fn type-name :def-tuple))

(defun def-tuple-getter (type-name)
  "Create an access macro such as (vector3d vec) that takes an instance of an array and unpacks it to tuples (aka multiple values)"
  (tuple-expansion-fn type-name :def-tuple-getter))

(defun def-tuple-array-getter (type-name)
  "Create a tuple aref macro for unpacking individual tuple from an array of tuples. eg (vector3d-aref up 5) => (values 0.0 1.0 0.0)"
  (tuple-expansion-fn type-name :def-tuple-array-getter))

(defun def-with-tuple (type-name)
  "Create a macro that can be used to bind members of a value tuple to symbols to symbols e-g (with-vector thing-vec (x y z w)  &body forms)"
  (tuple-expansion-fn type-name :def-with-tuple))

(defun def-with-tuple* (type-name)
  "Create a macro that can be used to bind members of the tuples array to symbols to symbols e-g (with-vector* thing-vec #(x y z w)  &body forms)"
  (tuple-expansion-fn type-name :def-with-tuple*))

(defun def-with-tuple-aref (type-name)
  "Create a macro that can be used to bind elements of an array of tuples to symbols to symbols e-g (with-vector3d-aref (thing-vec 5 (x y z w))  (+ x y z w))"
  (tuple-expansion-fn type-name :def-with-tuple-aref))

(defun def-tuple-setter (type-name)
  "Creates a tuple-setter for setting a tuple array from a mutiple-value tuple. eg (vector3d-setter up #{ 0.0 1.0 0.0 })"
  (tuple-expansion-fn type-name :def-tuple-setter))

(defun def-tuple-aref-setter (type-name)
  "Create an aref-setter macro for setting an element in an array of tuples  from a multiple-value tuple. eg (vector3d-aref-setter up 2 #{ 0.0 1.0 0.0 })"
  (tuple-expansion-fn type-name :def-tuple-aref-setter))

(defun def-new-tuple (type-name)
  "Create a function to create an array suitable for holding an individual tuple. eg (new-vector3d)"
  (tuple-expansion-fn type-name :def-new-tuple))

(defun def-tuple-maker (type-name)
  "Create a function to create an array suitable for holding an individual tuple, and initialise elements from multiple-value tuple. eg (make-vector3d #{ 1.0 2.0 2.0 })"
  (tuple-expansion-fn type-name :def-tuple-maker))

(defun def-tuple-maker* (type-name)
  "Create a function to create an array suitable for holding an individual tuple, and initialise elements from array tuple. eg (make-vector3d* #( 1.0 2.0 2.0 ))"
  (tuple-expansion-fn type-name :def-tuple-maker*))

(defun def-tuple-array-maker (type-name)
  "Create a function to create an array suitable for holding an number of individual tuples. ie an array of array tuples. eg (make-vector3d-array 5 :adjustable t)"
  (tuple-expansion-fn type-name :def-tuple-array-maker))

(defun def-tuple-array-dimensions (type-name)
  "Create a function that will return the number of tuples in the array of array tuples."
  (tuple-expansion-fn type-name :def-tuple-array-dimensions))

(defun def-tuple-setf (type-name)
  "Create generalised variable macros for tuple of type-name with the given elements."
  (tuple-expansion-fn type-name :def-tuple-setf))

(defun def-tuple-array-setf (type-name)
  (tuple-expansion-fn type-name :def-tuple-array-setf))

(defun def-tuple-map (type-name)
  "Creates a macro called maps-{tuple-type}-values. Which maps a the
function across a list of values, where it expects to recieve the same
number of values as the named type.
e.g (def-tuple-map vector2d) produces (map-vector2d-values fn &rest values)"
  (tuple-expansion-fn type-name :def-tuple-map))


(defun def-tuple-reduce (type-name)
  "Creates a macro called reduce-{tuple-type}-values. Which applies the reduction function to each value in it's second parameter, where it expects to recieve the same number of values as the named type. e.g (def-tuple-reduce vector2d) produces (reduce-vector2d-values fn tuples)"
  (tuple-expansion-fn type-name :def-tuple-reduce))

(defun document-tuple-type (type-name)  
  `(progn
     ;; instead of setf, need some form that can use the symbol in the format
     (setf (documentation ',(make-suffixed-symbol type-name "TUPLE") 'function) "Convert forms to tuple values.")
     (setf (documentation ',type-name 'function) "Unpack array  and convert to tuple values.")
     (setf (documentation ','(make-suffixed-symbol type-name "AREF") 'function) "Unpack individual tuple from an array of tuples.")           
     (setf (documentation ','(make-adorned-symbol type-name :prefix "WITH") 'function) "Bind members of the tuple values to symbols.")))


(defmacro make-tuple-operations (type-name)
  `(progn
     (def-tuple ',type-name)
     (def-tuple-array-dimensions ',type-name)
     (def-tuple-getter ',type-name)
     (def-tuple-array-getter ',type-name)
     (def-with-tuple ',type-name)
     (def-with-tuple* ',type-name)
     (def-with-tuple-aref ',type-name)
     (def-tuple-setter  ',type-name)
     (def-tuple-aref-setter  ',type-name)
     (def-new-tuple ',type-name)
     (def-tuple-maker ',type-name)
     (def-tuple-maker* ',type-name)
     (def-tuple-array-maker ',type-name)
     (def-tuple-setf  ',type-name)
     (def-tuple-array-setf  ',type-name)
     (def-tuple-map ',type-name)
     (def-tuple-reduce ',type-name)))

;; possibly we also need a deftype form to describe a tuple array?

(defmacro def-tuple-type (tuple-type-name &key tuple-element-type elements)
  "Create a tuple type. To be used from the top level. 
 For example (def-tuple-type vector3d single-float (x y z)) will create several macros and functions. Firstly, the accessor functions (vector3d array) (vector3d-aref array index). Secondly,  the context macros (with-vector3d tuple (element-symbols) forms..) and  (with-vector3d-array tuple (element-symbols) index forms..),  thirdly the constructors (new-vector3d) and (make-vector3d tuple),  (make-vector3d-array dimensions &key adjustable fill-pointer), forthly generalised access as in  (setf (vector3d array) tuple) and (setf (vector3d-aref array) index tuple), fiftly and finally, the  funcional macros (map-vector3d fn tuples..) (reduce-vector3d fn tuple)."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (make-tuple-symbol ',tuple-type-name ',tuple-element-type ',elements)
     (make-tuple-operations ,tuple-type-name)
     (document-tuple-type ',tuple-type-name)))


;; this needs some way of having the names as meaningful symbols
;; also a way of specifying type of return value and non-tuple parameters
(defmacro def-tuple-op (name args &body forms)
  "Macro to define a tuple operator. The name of the operator is
   name. The operator arguments are determined by args, which is a
   list of the form ((argument-name argument-type (elements)   ..)). 
   Within the forms the tuple value form is bound to the argument-name 
   and the tuple elements are bound to the symbols in the element list"
  (let ((arg-names (mapcar #'car args))
        (arg-typenames (mapcar #'cadr  args))
        (arg-elements (mapcar #'caddr args)))             
    `(progn
       (defmacro ,name ,arg-names 
         ,(arg-expander-fn arg-names arg-typenames arg-elements forms))
       ,(when (stringp (first forms))
         `(setf (documentation ',name 'function) ,(first forms))))))


