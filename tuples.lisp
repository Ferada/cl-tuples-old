;;;; tuples.lisp

(in-package :cl-tuples)

(defmacro def-tuple (type-name)
  "Create an macro such as (vector3d-tuple x y z) that takes element forms  and them into multiple values."
  `(defmacro ,(make-suffixed-symbol type-name "TUPLE") (&rest values-form)
     `(values
       ,@values-form)))


(defmacro def-tuple-getter (type-name)
  "Create an access macro such as (vector3d vec) that takes an instance of an array and unpacks it to tuples (aka multiple values)"
  `(defmacro ,type-name (tuple-array-name)
     `(values
       ,@(loop
            for index from 0 below (tuple-size ',type-name)
            collect
            `(aref ,tuple-array-name ,index)))))


(defmacro def-tuple-array-getter (type-name)
  "Create a tuple aref macro for packing multiple values into  a tuple struct. eg (vector! up #{ 0.0 1.0 0.0 })"
  `(defmacro ,(make-suffixed-symbol type-name "AREF") (target-sym array-index)
     (let* ((varlist (gensym-list ,(tuple-size type-name)))
            (array-index-sym (gensym))
            (counter-sym (gensym)))
       `(let ((,array-index-sym (* ,',(tuple-size type-name) ,array-index))
              (,counter-sym 0)) 
          (values ,@(mapcar #'(lambda (x)
                                (declare ( ignore x))
                                (list 
                                 'prog1 
                                 `(aref ,target-sym 
                                        (+ ,counter-sym ,array-index-sym)) 
                                 `(incf ,counter-sym)))
                            varlist))))))


(defmacro def-with-tuple (type-name)
  "Create a macro that can be used to bind members of the tuples  values to symbols to symbols e-g (with-vector thing-vec (x y z w)  &body forms)"
  `(defmacro ,(make-adorned-symbol type-name :prefix "WITH")  (tuple element-syms &body forms)
     `(multiple-value-bind
            ,element-syms
          ,tuple
        (progn ,@forms))))

(defmacro def-with-tuple* (type-name)
  "Create a macro that can be used to bind members of the tuples array to symbols to symbols e-g (with-vector* thing-vec (x y z w)  &body forms)"
  `(defmacro ,(make-adorned-symbol type-name :prefix "WITH" :asterisk t)  (tuple-array element-syms &body forms)
     `(multiple-value-bind
            ,element-syms
          (,',type-name ,tuple-array)
        (progn ,@forms))))

(defmacro def-with-tuple-array (type-name)
  "Create a macro that can be used to bind members of the tuples
   array to symbols to symbols e-g (with-vector thing-vec (x y z w)
   &body forms)"
  `(defmacro ,(make-adorned-symbol type-name :prefix "WITH" :suffix "ARRAY")  (tuple element-syms &optional index &body forms)
     `(symbol-macrolet
          ,(loop
              for element-sym in element-syms
              for array-index = (* index (tuple-size ',type-name))  then (1+ array-index)
              collect `(,element-sym (aref ,tuple ,index)))
        (progn
          ,@forms))))

(defmacro def-tuple-setter (type-name)
  "Creates a tuple-setter for setting values in a struct  a tuple struct. eg (vector3d-setter up #{ 0.0 1.0 0.0 })"
  `(defmacro ,(make-suffixed-symbol type-name "SETTER") (target-sym tuple-values)
     (let* ((varlist (gensym-list ,(tuple-size type-name))))
       `(multiple-value-bind
              ,varlist
            ,tuple-values
          (values
           ,@(loop
                for index from 0 below ,(tuple-size type-name)
                collect
                `(setf (aref ,target-sym ,index) ,(nth index varlist))))))))

(defmacro def-tuple-array-setter (type-name)
  "Create an aref-setter macro for packing multiple values into  a tuple struct. eg (vector3d-aref-setter up 2 #{ 0.0 1.0 0.0 })"
  `(defmacro ,(make-suffixed-symbol type-name "AREF-SETTER") (target-sym array-index tuple)
     (let* ((varlist (gensym-list ,(tuple-size type-name)))
            (array-index-sym (gensym))
            (counter-sym (gensym)))
       `(let ((,array-index-sym (* ,',(tuple-size type-name) ,array-index))
              (,counter-sym 0)) 
          (multiple-value-bind
                ,varlist
              ,tuple
            (values ,@(mapcar #'(lambda (x)
                                  (list 
                                   'prog1 
                                   `(setf (aref ,target-sym 
                                                (+ ,counter-sym ,array-index-sym)) ,x) 
                                   `(incf ,counter-sym)))
                              varlist)))))))

(defmacro def-tuple-creator (type-name)
  "Create a function to create an array suitable for holding an individual tuple."
    `(defmacro ,(make-prefixed-symbol type-name "NEW") ()
       `(make-array (list ,',(tuple-size type-name)) :element-type ',',(tuple-element-type type-name))))
  
(defmacro def-tuple-maker (type-name)
  "Create a function to create an array suitable for holding an individual tuple, and set its initial values"
    `(defmacro ,(make-prefixed-symbol type-name "MAKE") (&rest elements)
       (let ((tuple-sym (gensym)))
         `(let ((,tuple-sym (make-array (list ,',(tuple-size type-name)) :element-type ',',(tuple-element-type type-name))))
            (,',(make-suffixed-symbol type-name "SETTER") ,tuple-sym ,@elements)
            ,tuple-sym))))

(defmacro def-tuple-array-maker (type-name)
  "Create a function to create an array suitable for holding an number of individual tuples."
  `(defun ,(make-adorned-symbol  type-name :prefix "MAKE" :suffix "ARRAY") (dimensions &key adjustable fill-pointer)
     (make-array (* ,(tuple-size type-name) dimensions)
                 :adjustable adjustable
                 :fill-pointer fill-pointer
                 :element-type ',(tuple-element-type type-name))))

(defmacro def-tuple-array-dimensions (type-name)
  "Create a function that will return the size of the given tuple array (in tuples rather than tuple elements)"
  `(defun ,(make-adorned-symbol type-name :suffix "ARRAY-DIMENSIONS") (tuple-array)
     (/ (car  (array-dimensions tuple-array)) ,(tuple-size type-name))))

(defmacro def-tuple-setf (type-name)
  "Create generalised variable macros for tuple of type-name with the given elements"
  `(defsetf ,type-name ,(make-suffixed-symbol type-name "SETTER")))

(defmacro def-tuple-array-setf (type-name)
  `(defsetf ,(make-suffixed-symbol type-name "AREF")
       ,(make-suffixed-symbol type-name "AREF-SETTER")))

(defmacro map-values-aux (fn n gensym-lists &body v)
  (if v
      (let* ((gensym-list-sym (gensym-list n)))
        `(multiple-value-bind
               ,gensym-list-sym
             ,(car v)
           (map-values-aux ,fn ,n (,@gensym-lists ,gensym-list-sym) ,@(cdr v))))
      `(values
        ,@(loop
             for index from 0 below (length (car gensym-lists))
             collect
             `(funcall ,fn
                       ,@(loop
                            for gensym-list in gensym-lists
                            collect (nth index gensym-list)))))))


(defmacro map-values (fn n &body tuples)
  "Apply fn to each of the values in v, treating multiple value
sequences as map would do list sequences"
  `(map-values-aux ,fn ,n NIL ,@tuples))

(defmacro def-tuple-map (type-name)
  "Creates a macro called maps-{tuple-type}-values. Which maps a the
function across a list of values, where it expects to recieve the same
number of values as the named type.
e.g (def-tuple-map vector2d) produces (map-vector2d-values fn &rest values)"
  `(defmacro ,(make-adorned-symbol type-name :prefix "MAP" :suffix "TUPLES")  (fn &body tuples)
     `(map-values-aux ,fn ,(tuple-size ',type-name)  NIL ,@tuples)))


(defmacro reduce-values-body (fn n syms v)
  (cond
    ((= (length syms) 1)
     (list (car syms)))
    ((= (length syms) 2)
     `(funcall ,fn ,(car syms) ,(cadr syms)))
    ((> (length syms) 2)
     `(funcall ,fn ,(car syms) (reduce-values-body ,fn ,n ,(cdr syms) ,v)))))


(defmacro reduce-values (fn n v)
  "Reduce the values in v, to a single value by repeated application
of fn"
  (let*
      ((gensym-list-sym (gensym-list n)))
    `(multiple-value-bind
           ,gensym-list-sym
         ,v
       (reduce-values-body ,fn ,n ,gensym-list-sym ,v))))

(defmacro def-tuple-reduce (tuple-type)
  "Creates a macro called reduce-{tuple-type}-values. Which applies the reduction function to each value in it's second parameter, where it expects to recieve the same number of values as the named type. e.g (def-tuple-reduce vector2d) produces (reduce-vector2d-values fn tuples)"
  `(defmacro ,(make-adorned-symbol tuple-type :prefix "REDUCE" :suffix "TUPLE") (fn  tuples)
     `(reduce-values ,fn  ,(tuple-size ',tuple-type)  ,tuples)))

(defmacro export-tuple (type-name)
  `(mapcar #'export
           (list ',type-name
                 ',(make-prefixed-symbol type-name "MAKE")
                 ',(make-adorned-symbol  type-name :prefix "MAKE" :suffix "ARRAY")
                 ',(make-suffixed-symbol type-name "AREF")
                 
                 ',(make-adorned-symbol type-name :suffix "ARRAY-DIMENSIONS")
                 ',(make-adorned-symbol type-name :prefix "WITH")
                 ',(make-adorned-symbol type-name :prefix "WITH" :suffix "ARRAY")
                 
                 ',(make-suffixed-symbol  type-name "SETTER")
                 ',(make-suffixed-symbol type-name "AREF-SETTER")
                 ',(make-prefixed-symbol type-name "NEW")
                 ',(make-adorned-symbol  type-name :prefix "MAP" :suffix "TUPLES")
                 ',(make-adorned-symbol  type-name :prefix "REDUCE" :suffix "TUPLE"))))


(defmacro make-tuple-operations (type-name)
  `(progn
     (def-tuple ,type-name)
     (def-tuple-array-dimensions ,type-name)
     (def-tuple-getter ,type-name)
     (def-tuple-array-getter ,type-name)
     (def-with-tuple ,type-name)
     (def-with-tuple* ,type-name)
     (def-with-tuple-array ,type-name)
     (def-tuple-setter  ,type-name)
     (def-tuple-array-setter  ,type-name)
     (def-tuple-creator ,type-name)
     (def-tuple-maker ,type-name)
     (def-tuple-array-maker ,type-name)
     (def-tuple-setf  ,type-name)
     (def-tuple-array-setf  ,type-name)
     (def-tuple-map ,type-name)
     (def-tuple-reduce ,type-name)))


(defmacro def-tuple-type (tuple-type-name &key tuple-element-type elements)
  "Create a tuple type. To be used from the top level. 
 For example (def-tuple-type vector3d single-float (x y z)) will create several macros and functions. Firstly, the accessor functions (vector3d array) (vector3d-aref array index). Secondly,  the context macros (with-vector3d tuple (element-symbols) forms..) and  (with-vector3d-array tuple (element-symbols) index forms..),  thirdly the constructors (new-vector3d) and (make-vector3d tuple),  (make-vector3d-array dimensions &key adjustable fill-pointer), forthly generalised access as in  (setf (vector3d array) tuple) and (setf (vector3d-aref array) index tuple), fiftly and finally, the  funcional macros (map-vector3d fn tuples..) (reduce-vector3d fn tuple)."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (make-tuple-symbol ',tuple-type-name ',tuple-element-type ',elements)
     (make-tuple-operations ,tuple-type-name)
     (export-tuple ,tuple-type-name)))



(defmacro def-tuple-op (name  args &body forms)
  "Macro to define a tuple operator. The name of the operator is name. The operator arguments are determined by args, 
   which is a list of the form ((argument-name argument-type (elements) ..)). Within the forms 
   the tuple value form is bound to the argument-name and the tuple elements are bound to the symbols in the element list"
  (let ((arg-names (mapcar #'car args))
        (arg-typenames (mapcar #'cadr  args))
        (arg-elements (mapcar #'caddr args)))             
    `(defmacro ,name ,arg-names 
         ,(arg-expander-fn arg-names arg-typenames arg-elements forms))))


