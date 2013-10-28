;;;; tuples.lisp

(in-package :cl-tuples)

(defmacro def-tuple (type-name)
  "Create an alias for values for this tuple.eg (vector3d-values* 1.0 0.0 0.0) => #{ 1.0 0.0 0.0 }"
  (tuple-expansion-fn type-name :def-tuple-values))

(defmacro def-tuple-key (type-name)
  "Create an alias for values for this tuple.eg (vector3d-key-values z 1.0 x 2.0) => #{ 2.0 0.0 1.0 }"
  (tuple-expansion-fn type-name :def-tuple-key-values))

(defmacro def-tuple-typespec (type-name)
  "Create an alias typespec eg. (deftype vector3d* () `(values 'single-float 'single-float 'single-float))"
  (tuple-expansion-fn type-name :def-tuple-type))

(defmacro def-tuple-array-typespec (type-name)
  (tuple-expansion-fn type-name :def-tuple-array-type))

(defmacro def-tuple-struct (type-name)
  (tuple-expansion-fn type-name :def-tuple-struct))

(defmacro def-tuple-getter (type-name)
  "Create an access macro such as (vector3d vec) that takes a tuple place and unpacks it to tuples (aka multiple values)"
  (tuple-expansion-fn type-name :def-tuple-getter))

(defmacro def-tuple-set (type-name)
  (tuple-expansion-fn type-name :def-tuple-set))

(defmacro def-tuple-aref (type-name)
  "Create a tuple aref macro for unpacking individual tuple from an array of tuples. eg (vector3d-aref up 5) => #(0.0 1.0 0.0)"
  (tuple-expansion-fn type-name :def-tuple-aref))

(defmacro def-tuple-aref* (type-name)
  "Create a tuple aref macro for unpacking individual tuple from an array of tuples. eg (vector3d-aref up 5) => (values 0.0 1.0 0.0)"
  (tuple-expansion-fn type-name :def-tuple-aref*))

(defmacro def-with-tuple (type-name)
  "Create a macro that can be used to bind members of a value tuple to symbols to symbols e-g (with-vector thing-vec (x y z w)  &body forms)"
  (tuple-expansion-fn type-name :def-with-tuple))

(defmacro def-with-tuple* (type-name)
  "Create a macro that can be used to bind members of the tuples place to symbols to symbols e-g (with-vector* thing-vec #(x y z w)  &body forms)"
  (tuple-expansion-fn type-name :def-with-tuple*))

(defmacro def-with-tuple-aref (type-name)
  "Create a macro that can be used to bind elements of an array of tuples to symbols e-g (with-vector3d-aref (thing-vec 5 (x y z w))  (+ x y z w))"
  (tuple-expansion-fn type-name :def-with-tuple-aref))

(defmacro def-tuple-setter (type-name)
  "Creates a tuple-setter for setting a tuple place from a mutiple-value tuple. eg (vector3d-setter up #{ 0.0 1.0 0.0 })"
  (tuple-expansion-fn type-name :def-tuple-setter))

(defmacro def-tuple-aref-setter (type-name)
  "Create an aref-setter macro for setting an element in an array of tuples from a multiple-value tuple. eg (vector3d-aref-setter up 2 #( 0.0 1.0 0.0 ))"
  (tuple-expansion-fn type-name :def-tuple-aref-setter))

(defmacro def-tuple-aref-setter* (type-name)
  "Create an aref-setter macro for setting an element in an array of tuples from a multiple-value tuple. eg (vector3d-aref-setter up 2 #{ 0.0 1.0 0.0 })"
  (tuple-expansion-fn type-name :def-tuple-aref-setter*))

(defmacro def-tuple-vector-push (type-name)
    (tuple-expansion-fn type-name :def-tuple-vector-push))

(defmacro def-tuple-vector-push-extend (type-name)
    (tuple-expansion-fn type-name :def-tuple-vector-push-extend))

(defmacro def-tuple-vector-push* (type-name)
    (tuple-expansion-fn type-name :def-tuple-vector-push*))

(defmacro def-tuple-vector-push-extend* (type-name)
    (tuple-expansion-fn type-name :def-tuple-vector-push-extend*))

(defmacro def-new-tuple (type-name)
  "Create a function to create a place suitable for holding an individual tuple. eg (new-vector3d)"
  (tuple-expansion-fn type-name :def-new-tuple))

(defmacro def-tuple-maker (type-name)
  "Create a function to create an place suitable for holding an individual tuple, and initialise elements from multiple-value tuple. eg (make-vector3d (values 1.0 2.0 2.0 ))"
  (tuple-expansion-fn type-name :def-tuple-maker))

(defmacro def-tuple-maker* (type-name)
  "Create a function to create an place suitable for holding an individual tuple, and initialise elements from array tuple. eg (make-vector3d* #( 1.0 2.0 2.0 ))"
  (tuple-expansion-fn type-name :def-tuple-maker*))

(defmacro def-tuple-array-maker (type-name)
  "Create a function to create an array suitable for holding an number of individual tuples. ie an array of tuple places. eg (make-vector3d-array 5 :adjustable t)"
  (tuple-expansion-fn type-name :def-tuple-array-maker))

(defmacro def-tuple-array-dimensions (type-name)
  "Create a function that will return the number of tuples in the array of tuple places."
  (tuple-expansion-fn type-name :def-tuple-array-dimensions))

(defmacro def-tuple-fill-pointer (type-name)
  "Create a function that will return a vector fill pointer in terms of tuple size"
  (tuple-expansion-fn type-name :def-tuple-fill-pointer))

(defmacro def-tuple-setf-fill-pointer (type-name)
  "Create a function that will adjust a vector fill pointer in terms of tuple size"
  (tuple-expansion-fn type-name :def-tuple-setf-fill-pointer))

(defmacro def-tuple-setf* (type-name)
  "Create generalised variable macros for tuple of type-name with the given elements."
  (tuple-expansion-fn type-name :def-tuple-setf*))

(defmacro def-tuple-array-setf* (type-name)
  "Create generalised variable macros for an array of  tuples of type-name with the given elements."
  (tuple-expansion-fn type-name :def-tuple-array-setf*))

(defmacro def-tuple-array-setf (type-name)
  "Create generalised variable macros for an array of  tuples of type-name with the given elements."
  (tuple-expansion-fn type-name :def-tuple-array-setf))

(defmacro def-tuple-map (type-name)
  (tuple-expansion-fn type-name :def-tuple-map))

(defmacro def-tuple-reduce (type-name)
  (tuple-expansion-fn type-name :def-tuple-reduce))

(defun document-tuple-type (type-name)
  `(progn
     ;; instead of setf, need some form that can use the symbol in the format
     (setf (documentation ',(tuple-symbol type-name :def-tuple-values) 'function)
           (format nil "Convert ~A forms to multiple values." ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-tuple-getter) 'function)
           (format nil "Unpack array representation of an ~A and convert to multiple values." ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-tuple-aref*) 'function)
           (format nil "Unpack individual ~A to multiple values from an array of ~As." ,(string type-name) ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-with-tuple) 'function)
           (format nil "Bind elements of a ~A multiple value to symbols."  ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-with-tuple*) 'function)
           (format nil "Bind elements of a ~A vector to symbols."  ',(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-with-tuple-aref) 'function)
           (format nil  "Bind the elements of a ~A from vector of ~A's to symbols" ,(string type-name) ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-tuple-setter) 'function)
           (format nil "Creates a macro for setting an ~A vector from a multiple values ~A" ,(string type-name) ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-tuple-aref-setter*) 'function)
           (format nil "Creates a macro for setting an ~A vector in a vector of ~As from a multiple values ~A" ,(string type-name) ,(string type-name) ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-tuple-vector-push*) 'function)
           (format nil "Push a ~A multiple value onto the end of a vector of ~A's " ,(string type-name) ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-tuple-vector-push-extend*) 'function)
           (format nil  "Push a ~A multiple value onto the end of a vector of ~A's with the possibility of extension" ,(string type-name) ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-new-tuple) 'function)
           (format nil  "Create an array suitable for holding a single ~A" ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-tuple-maker) 'function)
           (format nil  "Create an array sutable for holding a single ~A and initialize it from a multiple-values form" ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-tuple-maker*) 'function)
           (format nil   "Create an array sutable for holding a single ~A and initialize it from a  form" ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-tuple-array-maker) 'function)
           (format nil  "Create an array suitable for holding a number of ~A's " ,(string type-name)))
     (setf (documentation ',(tuple-symbol type-name :def-tuple-array-dimensions) 'function)
           (format nil  "Return the size of a vector of ~A's (ie how many ~A's it contains)" ,(string type-name) ,(string type-name)))
     (values)))

(defmacro def-tuple-documentation (type-name)
  (document-tuple-type type-name))

(defmacro make-tuple-operations (type-name)
  `(progn
     (def-tuple ,type-name)
     (def-tuple-key ,type-name)
	 (def-tuple-struct ,type-name)
     (def-tuple-getter ,type-name)
     (def-tuple-aref* ,type-name)
	 (def-tuple-aref ,type-name)
     (def-tuple-aref-setter*  ,type-name)
	 (def-tuple-aref-setter ,type-name)
     (def-tuple-array-dimensions ,type-name)
     (def-tuple-fill-pointer ,type-name)
     (def-tuple-setf-fill-pointer ,type-name)
     (def-with-tuple ,type-name)
     (def-with-tuple* ,type-name)
     (def-with-tuple-aref ,type-name)
     (def-tuple-setter  ,type-name)
     (def-tuple-vector-push ,type-name)
     (def-tuple-vector-push-extend ,type-name)
     (def-tuple-vector-push* ,type-name)
     (def-tuple-vector-push-extend* ,type-name)
     (def-new-tuple ,type-name)
     (def-tuple-maker ,type-name)
     (def-tuple-maker* ,type-name)
     (def-tuple-array-maker ,type-name)
     (def-tuple-setf*  ,type-name)
     (def-tuple-array-setf*  ,type-name)
     (def-tuple-array-setf ,type-name)
     (def-tuple-map ,type-name)
     (def-tuple-reduce ,type-name)))

(defmacro export-tuple-operations (type-name)
  `(progn
     ,@(loop for kw in *tuple-expander-keywords* collect `(export (tuple-symbol (quote ,type-name) ,kw)))))


;; possibly we also need a deftype form to describe a tuple array?

(defmacro def-tuple-type (tuple-type-name &key tuple-element-type initial-element elements)
  "Create a tuple type. To be used from the top level.
 For example (def-tuple-type vector3d single-float (x y z)) will create several macros and functions. 
 Firstly, the accessor functions (vector3d array) (vector3d-aref array index). 
 Secondly, the context macros (with-vector3d tuple (element-symbols) forms..) and  (with-vector3d-array tuple (element-symbols) index forms..),  
 Thirdly the constructors (new-vector3d) and (make-vector3d tuple),  (make-vector3d-array dimensions &key adjustable fill-pointer), 
 Forthly generalised access as in  (setf (vector3d array) tuple) and (setf (vector3d-aref array) index tuple)," 
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (cl-tuples::make-tuple-symbol ',tuple-type-name ',tuple-element-type ',initial-element ',elements)
     (cl-tuples::make-tuple-operations ,tuple-type-name)
     (cl-tuples::def-tuple-documentation ,tuple-type-name)))


;; full syntax (def-tuple-op name ((name type (elements)) ..) (
;; this needs some way of having the names as meaningful symbols
;; also a way of specifying type of return value and non-tuple parameters
(defmacro def-tuple-op (name param-list  &body forms)
  "Macro to define a tuple operator. The name of the operator is
   name. The operator arguments are determined by args, which is a
   list of the form ((argument-name argument-type (elements)   ..)).
   Within the forms the tuple value form is bound to the argument-name
   and the tuple elements are bound to the symbols in the element list"
  (let* ((param-names (mapcar #'car param-list))
         (param-typenames (mapcar #'cadr  param-list))
         (param-elements (mapcar (lambda (param)
                                   (let* ((type-name (cadr param))
                                          (size (and (tuple-typep type-name) (tuple-size type-name)))
                                          (elements (caddr param)))
                                     (or
                                      (if (eq elements :default)
                                          (tuple-elements type-name)
                                          elements)
                                      (and size (make-gensym-list size)))))
                                 param-list))
         (doc (if (stringp (first forms))
                  (first forms)
                  (format nil "DEF-TUPLE-OP ~A ~A" name param-typenames))))
    `(defmacro ,name ,param-names
       ,doc
       ,(def-tuple-expander-fn param-names param-typenames param-elements forms))))
