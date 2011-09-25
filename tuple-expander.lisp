
(in-package :cl-tuples)

(defun construct-tuple-array-reference (type-name tuple-array-name index)
  "Given a tuple type and an array index, return a form for returnign the array index"
  `(the ,(tuple-element-type type-name) (aref (the ,(tuple-typespec* type-name) ,tuple-array-name) ,index)))

(defun construct-tuple-value-type (type-name)
  "Given a tuple name construct the for of a  value typespec for it eg (values single-float single-float)"
  `(values ,@(loop for i from 0 below (tuple-size type-name) collect (tuple-element-type type-name))))

(defun construct-tuple-slots (type-name)
  "Given a tuple type return a list of slots sutable for the body of a defstruct body eg: ((A 0.0 :TYPE SINGLE-FLOAT) (B 0.0 :TYPE SINGLE-FLOAT))"
  (loop for e in (tuple-elements type-name)
	 collect
	   (list e (tuple-initial-element type-name) :type (tuple-element-type type-name))))

(defparameter *tuple-expander-keywords*
  '(:def-tuple-type :def-tuple-array-type
	:def-tuple-struct
	:def-tuple-getter :def-tuple-aref* :def-tuple-aref
	:def-nth-tuple
	:def-with-tuple :def-with-tuple* :def-with-tuple-aref
	:def-tuple-set :def-tuple-setter :def-tuple-aref-setter*
	:def-tuple-aref-setter
	:def-tuple-vector-push  :def-tuple-vector-push-extend
	:def-tuple-vector-push* :def-tuple-vector-push-extend*
	:def-new-tuple  :def-tuple-maker
	:def-tuple-maker*
	:def-tuple-array-maker :def-tuple-array-dimensions
	:def-tuple-setf* :def-tuple-array-setf*
	:def-tuple-array-setf))

(defgeneric tuple-symbol (type-name expansion))

(defgeneric tuple-expansion-fn (type-name expansion))

;; eg. (vector3d-values* 1 2 3) => #{ 1 2 3 }
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple)))
  (make-adorned-symbol type-name :suffix "VALUES" :asterisk t ))

;; eg (vector3d-values 1.2 3.0 1.2) => #{ 1.2 3.0 1.2 }
(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple)))
  "Expand to a macro that will create a values form representing our tuple type."
  `(defmacro ,(tuple-symbol type-name expansion) (&rest elements)
	 `(the ,',(construct-tuple-value-type type-name)
		(values  ,@elements))))

;; deftype form for the multiple value equivalent of the struct
;; eg. (deftype vector3d* () `(values single-float single-float single-float))
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-type)))
  (make-adorned-symbol type-name :asterisk t))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-type)))
  "Expand the tuple multiple value deftype form."
  `(deftype ,(tuple-symbol type-name expansion) ()
	 ,(construct-tuple-value-type type-name)))

;; deftype form for the array equivalent of the struct
;; eg (deftype vector3d-array () (vector single-float *))
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-array-type)))
  (make-adorned-symbol type-name :suffix "ARRAY"))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-array-type)))
  "Expand the deftype form for an array of tuples"
  `(deftype ,(tuple-symbol type-name expansion) ()
	 (vector ,(tuple-element-type type-name) *)))

;; deftype form to generate the structure definition eg (defstruct vector3d ..)
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-struct)))
  (make-adorned-symbol type-name))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-struct)))
  "Defines a structure that will hold the tuple based on a vector."
  `(defstruct (,(tuple-symbol type-name expansion) (:type vector) (:constructor nil))
	 ,@(construct-tuple-slots type-name)))

;; -- generalised access --

;; return macro that will convert an array to a values form (vector3d* v) => #{ x y z }
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-getter)))
  (make-adorned-symbol type-name :asterisk t))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-getter)))
  "Create a macro that will return the contents of a place representing our tuple as a value form"
  `(defmacro ,(tuple-symbol type-name :def-tuple-getter) (tuple-array-name)
	 `(the ,(construct-tuple-value-type ',type-name)
		(values
		 ,@(loop
			  for index from 0 below (tuple-size ',type-name)
			  collect
				(construct-tuple-array-reference ',type-name tuple-array-name index))))))


;; generate a setter for use with setf that takes values and places them into an array eg (vector3d-setter v #{ 1 2 3 })
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-setter)))
  (make-adorned-symbol type-name :suffix "SETTER" :asterisk t))

(defun construct-tuple-set-aref (type-name tuple-place index varlist)
  `(setf (aref (the ,(tuple-typespec* type-name) ,tuple-place) ,index) ,(nth index varlist)))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-setter)))
  "Create a macro that will set an tuple place form to the values of a tuple value form"
  `(defmacro ,(tuple-symbol type-name :def-tuple-setter) (tuple-place tuple-values)
	 (let* ((varlist (gensym-list ,(tuple-size type-name))))
	   `(multiple-value-bind
			  ,varlist
			,tuple-values
		  (declare (type ,',(tuple-element-type type-name) ,@varlist))
		  (values
		   ,@(loop
				for index from 0 below ,(tuple-size type-name)
				collect
				  (construct-tuple-set-aref ',type-name tuple-place index varlist)))))))

;; generate a setter for use with setf that takes values and places them into an array eg (vector3d-setter v #{ 1 2 3 })
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-set)))
  (make-adorned-symbol type-name :prefix "SET"))

;; to do -- this needs type declarations
(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-set)))
  `(defun ,(tuple-symbol type-name :def-tuple-set) (tuple-place ,@(tuple-elements type-name))
	 ,@(loop
		  for index from 0 below (tuple-size type-name)
		  collect
			`(setf (aref tuple-place ,index) ,(nth index (tuple-elements type-name))))))

;; generalised reference to a tuple place
;;(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-setf*)))
;;  (make-adorned-symbol type-name :suffix "SETTER"))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-setf*)))
  "Expand form that creates generalized reference to a tuple place"
  `(defsetf ,(tuple-symbol type-name :def-tuple-getter) ,(tuple-symbol type-name :def-tuple-setter)))

;; -- arrays --

;; to do -- possibly re-engineer as a macro

;; create a flat array dimensioned to hold n tuples eg. (make-vector3d-array 3 :adjustable t)
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-array-maker)))
  (make-adorned-symbol type-name :prefix "MAKE" :suffix "ARRAY" ))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-array-maker)))
  "Create macro that creates a array of tuple array places."
  `(defun ,(tuple-symbol type-name :def-tuple-array-maker) (dimensions &key adjustable initial-element fill-pointer)
	 (the ,(tuple-typespec** type-name)
	   (make-array (* ,(tuple-size type-name) dimensions)
				   :adjustable adjustable
				   :initial-element (when initial-element ,(tuple-initial-element type-name))
				   :fill-pointer (when fill-pointer (* ,(tuple-size type-name) fill-pointer))
				   :element-type ',(tuple-element-type type-name)))))

;; create an array accessor that accesses an array of tuples and produces a value form eg (vector3d-aref* vecs 2) => #{ 2.3 4.3 2.4 }
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-aref*)))
  (make-adorned-symbol type-name :suffix "AREF" :asterisk t))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-aref*)))
  "Create a macro that will index an array that is considered to be an array of tuples and extract an individual tuple as a value form"
  `(defmacro ,(tuple-symbol type-name :def-tuple-aref*) (tuple-array array-index)
	 (let* ((varlist (gensym-list ,(tuple-size type-name)))
			(array-index-sym (gensym)))
	   `(let ((,array-index-sym (* ,',(tuple-size type-name) ,array-index)))
		  (the ,',(tuple-typespec type-name)
			(values ,@(let ((counter 0))
						   (mapcar #'(lambda (x)
									   (declare (ignore x))
									   (prog1
										   `(aref (the ,',(tuple-typespec** type-name) ,tuple-array)
												  (+ ,counter ,array-index-sym))
										 (incf counter)))
								   varlist))))))))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-nth-tuple)))
  (make-adorned-symbol type-name :prefix "NTH"))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-nth-tuple)))
  `(defun ,(tuple-symbol type-name :def-tuple-setter) (index tuple-place)
	 (make-array ,(tuple-size type-name) :displaced-to tuple-place :displaced-index-offset (* ,(tuple-size type-name) index))))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-aref-setter)))
  (make-adorned-symbol type-name :suffix "AREF-SETTER"))
	
(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-aref-setter)))
  "Create a macro that will set an indexed array of tuple places to the values of a tuple struct form"
  `(defun ,(tuple-symbol type-name :def-tuple-aref-setter) (tuple-name tuple-index tuple)
	 (setf (subseq tuple-name
				   (* ,(tuple-size type-name) tuple-index)
				   (* ,(tuple-size type-name) (1+ tuple-index)))
		   tuple)))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-aref)))
  (make-adorned-symbol type-name :suffix "AREF"))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-aref)))
  "Create a macro that will set an indexed array of tuple places to the values of a tuple struct form"
  `(defun  ,(tuple-symbol type-name :def-tuple-aref) (tuple-array tuple-index)
	 (subseq tuple-array 
			 (* ,(tuple-size type-name) tuple-index)
			 (* ,(tuple-size type-name) (1+ tuple-index)))))
		  

;; create a setter macro (for generalised setf places) that will set a tuple value form into an indexed array
;; eg (vector3d-aref-setter vecs 2 #{ 2.3 2.3 4.2 })
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-aref-setter*)))
  (make-adorned-symbol type-name :suffix "AREF-SETTER" :asterisk t))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-aref-setter*)))
  "Create a macro that will set an indexed array of tuple places to the values of a tuple value form"
  `(defmacro ,(tuple-symbol type-name :def-tuple-aref-setter*) (array-name array-index tuple-values)
	 (let* ((varlist (gensym-list ,(tuple-size type-name)))
			(array-index-sym (gensym)))
	   `(let ((,array-index-sym (* ,',(tuple-size type-name) ,array-index)))
		  (multiple-value-bind
				,varlist
			  ,tuple-values
			(declare (type ,',(tuple-element-type type-name) ,@varlist))
			(values ,@(let ((counter 0))
						   (mapcar #'(lambda (x)
									   (prog1
										   `(setf (aref (the ,',(tuple-typespec** type-name) ,array-name)
														(+ ,counter ,array-index-sym)) (the ,',(tuple-element-type type-name) ,x))
										 (incf counter)))
								   varlist))))))))

;; generalised reference to an array of tuples via value forms
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-array-setf*)))
  (make-adorned-symbol type-name :suffix "AREF" ))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-array-setf*)))
  "Expand form that creates generalized reference to tuple-arrays"
  `(defsetf ,(tuple-symbol type-name :def-tuple-aref*)
	   ,(tuple-symbol type-name :def-tuple-aref-setter*)))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-array-setf)))
  "Expand form that creates generalized reference to tuple-arrays"
  `(defsetf ,(tuple-symbol type-name :def-tuple-aref)
	   ,(tuple-symbol type-name :def-tuple-aref-setter)))

;; create a function that returns the dimensions of an array scaled down to tuple units
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-array-dimensions)))
  (make-adorned-symbol type-name :suffix "ARRAY-DIMENSIONS"))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-array-dimensions)))
  "Create macro that returns the number of tuples in an array of tuple places."
  `(defun ,(tuple-symbol  type-name :def-tuple-array-dimensions) (tuple-array)
	 (the fixnum (/ (the fixnum (length tuple-array)) (the fixnum ,(tuple-size type-name))))))

;; --- vectors --

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-vector-push)))
  (make-adorned-symbol type-name :suffix "VECTOR-PUSH"))

;; tuple-vector-push
(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-vector-push)))
  "Create a macro that will push a tuple value form into an array of existing tuple places."
  `(defun ,(tuple-symbol type-name :def-tuple-vector-push) (tuple array-name)
	 (declare (type ,(tuple-typespec* type-name)) (type ,(tuple-typespec** type-name)))
	 (loop
		for index from 0 below ,(tuple-size type-name)
		do (vector-push (aref tuple index) array-name))))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-vector-push-extend)))
  (make-adorned-symbol type-name :suffix "VECTOR-PUSH-EXTEND"))

;; tuple-vector-push
(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-vector-push-extend)))
  "Create a macro that will push a tuple value form into an array of existing tuple places."
  `(defun ,(tuple-symbol type-name :def-tuple-vector-push-extend) (tuple array-name)
	 (declare (type ,(tuple-typespec* ,type-name) (type ,(tuple-typespec** ,type-name)))
	 (loop
		for index from 0 below ,(tuple-size type-name)
		do (vector-push-extend (aref tuple index) array-name))))

;; eg. (vector3d-push* vecs #{ 0.0 1.0 3.0 })
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-vector-push*)))
  (make-adorned-symbol type-name :suffix "VECTOR-PUSH" :asterisk t))

;; tuple-vector-push
(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-vector-push*)))
  "Create a macro that will push a tuple value form into an array of existing tuple places."
  `(defmacro ,(tuple-symbol type-name :def-tuple-vector-push*) (tuple-values array-name)
	 (let* ((varlist (gensym-list ,(tuple-size type-name))))
	   `(multiple-value-bind
			  ,varlist
			,tuple-values
		  (declare (type ,',(tuple-element-type type-name) ,@varlist))
		  ,@(loop
			   for index from 0 below ,(tuple-size type-name)
			   collect
				 `(vector-push ,(nth index varlist) (the ,',(tuple-typespec** type-name) ,array-name)))))))

;; eg. (vector3d-push-extend* vecs #{ 0.0 1.0 3.0 })
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-vector-push-extend*)))
  (make-adorned-symbol type-name :suffix "VECTOR-PUSH-EXTEND" :asterisk t))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-vector-push-extend*)))
  "Create a macro that will push a tuple value form into an array of existing tuple places, extending if adjustable."
  `(defmacro ,(tuple-symbol type-name :def-tuple-vector-push-extend*) (tuple-values array-name)
	 (let* ((varlist (gensym-list ,(tuple-size type-name))))
	   `(multiple-value-bind
			  ,varlist
			,tuple-values
		  (declare (type ,',(tuple-element-type type-name) ,@varlist))
		  ,@(loop
			   for index from 0 below ,(tuple-size type-name)
			   collect
				 `(vector-push-extend ,(nth index varlist) (the ,',(tuple-typespec** type-name) ,array-name) ,',(tuple-size type-name)))))))

;; -- bindings --
;; bind tuple values to symbols during evaluation of the form eg (with-vector3d #{ 1.0 2.0 3.0 } (x y z) (fomat t "~A" (list x y z)))
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-with-tuple*)))
  (make-adorned-symbol type-name :prefix "WITH" :asterisk t))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-with-tuple*)))
  "Create a wrapper that will bind a tuple value form to symbols during evaluation of the body."
  `(defmacro ,(tuple-symbol type-name :def-with-tuple*)  (tuple element-syms &body forms)
	 (assert (= (length element-syms) ,(tuple-size type-name)) nil "Incorrect length element-syms supplied to with-tuple")
	 `(multiple-value-bind
			,element-syms
		  ,tuple
		(declare (ignorable ,@element-syms) (type ,',(tuple-element-type type-name) ,@element-syms))
		(progn ,@forms))))

;; bind tuple vector to symbols during evaluation of the form eg (with-vector3d #( 1.0 2.0 3.0 ) (x y z) (fomat t "~A" (list x y z)))
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-with-tuple)))
  (make-adorned-symbol type-name :prefix "WITH"))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql  :def-with-tuple)))
  "Create a wrapper that will bind a tuple place  to symbols during evaluation of the body."
  `(defmacro ,(tuple-symbol type-name :def-with-tuple) (tuple-place element-syms &body forms)
	 (assert (= (length element-syms) ,(tuple-size type-name)) nil "Incorrect length element-syms supplied to with-tuple*")
	 ` (multiple-value-bind
			 ,element-syms
		   (values ,@(let ((counter 0))
						  (mapcar #'(lambda (x)
									  (declare (ignore x))
									  (prog1
										  `(aref (the ,',(tuple-typespec** type-name) ,tuple-place) ,counter)
										(incf counter)))
								  element-syms)))
		 (declare (ignorable ,@element-syms) (type ,',(tuple-element-type type-name) ,@element-syms))
		 (progn ,@forms))))

;; bind tuple array elements to symbols during evaluation of the form eg (with-vector3d-aref (vecs 2 (x y z)) (fomat t "~A" (list x y z)))
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-with-tuple-aref)))
  (make-adorned-symbol type-name :prefix "WITH" :suffix "AREF" ))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-with-tuple-aref)))
  "Create a wrapper macro that will bind an indexed tuple form in an array to symbols turing evaluation of the body."
  `(defmacro ,(tuple-symbol type-name :def-with-tuple-aref)  ((array-name index element-syms) &body forms)
	 (assert (= (length element-syms) ,(tuple-size type-name)) nil "Incorrect length element-syms supplied to with-tuple-aref")
	 (let* ((array-index-sym (gensym)))
	   `(let ((,array-index-sym (* ,',(tuple-size type-name) ,index)))
		  (multiple-value-bind
				,element-syms
			  ;; this is the bit we need to generate
			  (values ,@(let ((counter 0))
							 (mapcar #'(lambda (x)
										 (declare (ignore x))
										 (prog1
											 `(aref (the ,',(tuple-typespec** type-name) ,array-name) (+ ,counter ,array-index-sym))
										   (incf counter)))
									 element-syms)))
			(declare (ignorable ,@element-syms) (type ,',(tuple-element-type type-name)))
			(progn ,@forms))))))

;; -- constructors --
(defun construct-tuple-array-maker (type-name)
  `(make-array  ,(tuple-size type-name) :initial-element ,(tuple-initial-element type-name) :element-type ',(tuple-element-type type-name)))

;; create a new tuple, freshly initialised eg (new-vector3d) => #( 0.0 0.0 0.0 )
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-new-tuple)))
  (make-adorned-symbol type-name :prefix "NEW" ))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-new-tuple)))
  "Create a macro that creates a new tuple."
  `(defmacro ,(tuple-symbol type-name :def-new-tuple) ()
	 `(the ,',(tuple-typespec* type-name)
		,(construct-tuple-array-maker ',type-name))))


;; create and initalise a tupe eg (make-vector3d 0.0 1.0 2.0) => #( 0.0 1.0 2.0 )
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-maker)))
  (make-adorned-symbol type-name :prefix "MAKE"))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-maker)))
  "Create a macro that creates new tuple place and initialize it from a list of elements"
  `(defmacro ,(tuple-symbol type-name :def-tuple-maker) (&rest elements)
	 (assert (= (length elements) ,(tuple-size type-name)))
	 (let ((tuple-sym (gensym)))
	   `(let ((,tuple-sym
			   ,(construct-tuple-array-maker ',type-name)))
		  (declare (type ,',(tuple-typespec* type-name) ,tuple-sym))
		  (,',(tuple-symbol type-name :def-tuple-setter) ,tuple-sym (values  ,@elements))
		  ,tuple-sym))))


;; --- create and initialise from multiple values
(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-maker*)))
  (make-adorned-symbol type-name :prefix "MAKE" :asterisk t))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-maker*)))
  "Create a macro that creates new tuple place, form and initialize it with values"
  `(defmacro ,(tuple-symbol type-name :def-tuple-maker*) (tuple-values)
	 (let ((varlist (gensym-list ,(tuple-size type-name)))
		   (tuple-sym (gensym))
		   (counter-sym 0))
	   (declare (type fixnum counter-sym))
	   `(let  ((,tuple-sym
				,(construct-tuple-array-maker ',type-name)))
		  (declare (type ,',(tuple-typespec* type-name) ,tuple-sym))
		  (multiple-value-bind
				,varlist
			  ,tuple-values
			(declare (type ,',(tuple-element-type type-name) ,@varlist))
			(progn ,@(mapcar #'(lambda (x)
								 (prog1
									 `(setf (aref ,tuple-sym (the fixnum ,counter-sym)) ,x)
								   (incf counter-sym)))
							 varlist)
				   ,tuple-sym))))))


;; -- def-tuple-op expanders begin here ------------------------------------

;; (in-package :tuple-types)

;; (defclass %tuple-fun ()
;;   ((name :intiarg :fun-name)
;;    (params :initarg :params)
;;    (types :initarg :types)
;;    (elements :initarg :elements)))

;; (in-package :cl-tuples)

(defun symbol-macro-expander-fn (n names types elements gensyms body)
  "Wrap the body of def tuple op in symbol macros mapped to gensyms to prevent
   name capture."
  ;; if this is a tuple type with elements, we expand using with-tuple
  (if (tuple-typep (nth n types))
	  (progn
		(assert (= (length (nth n gensyms))
				   (length (nth n elements)))
				nil "~A contains too few elements for a ~A" (nth n elements) (nth n types))
		``(symbol-macrolet
			  ,',(loop
					for gensym in (nth n gensyms)
					for element in (nth n elements) collect `(,element  ,gensym))
			(declare (ignorable ,@',(nth n gensyms)))
			(symbol-macrolet ((,',(nth n names) (,',(make-adorned-symbol (nth n types) :asterisk t)
													,@',(loop
														   for gensym in (nth n gensyms)
														   collect gensym))))
			  ;; recurs down to the next parameter
			  ,,(if (< (1+ n) (length names))
					(symbol-macro-expander-fn (1+ n) names types elements gensyms body)
					;; or bottom out
					``(progn ,@',body)))))
	  ;; if this is not a tuple type, and theres more to come, recurse down
	  (if (< (1+ n) (length names))
		  (symbol-macro-expander-fn (1+ n) names types elements gensyms body)
		  ;; otherwise, bottom out
		  ``(progn ,@',body))))

(defun arg-expander-fn-aux (n names types elements gensyms body)
  "Handle the expansion of the n-th parameter in a def-tuple-op call list"
  (if (nth n types)
	  ;; if it's a tuple type, bind to gensyms using the apropiate with-tuple macro
	  (if (tuple-typep (nth n types))
		  ``(,',(make-adorned-symbol (nth n types) :prefix "WITH")
				,,(nth n  names) ,',(nth n  gensyms)
				,,(if (< (1+ n) (length names))
					  (arg-expander-fn-aux (1+ n) names types elements gensyms body)
					  (symbol-macro-expander-fn 0 names types elements gensyms body)))
		  ;; otherwise just use a straight symbol
		  ``(symbol-macrolet ((,',(nth n names) (the ,',(nth n types)  ,,(nth n names))))
			  ,,(if (< (1+ n) (length names))
					(arg-expander-fn-aux (1+ n) names types elements gensyms body)
					(symbol-macro-expander-fn 0 names types elements gensyms body))))
	  ;; if there are no associated parameters with this op, just expand the body
	  (symbol-macro-expander-fn 0 nil nil nil nil body)))


(defun body-expander-fn (names types elements gensyms body)
  "Expand the declarations and return type wrapper round a def-tuple-op. form"
  ;; have we specifed a return type?
  (if (eq (caar body) :return)
	  (let ((ret-type
			 ;; is it a tuple type?
			 (if (tuple-typep (cadar body))
				 ;; yes, expand into type spec
				 (tuple-typespec (cadar body))
				 ;; no, just use literal expansion
				 (cadar body)))
			;; the rest of the body is the actual body
			(real-body (cddar body)))
		;; when we have a parameter list, expand it
		``(the ,',ret-type
			,,(arg-expander-fn-aux 0 names types elements gensyms real-body)))
	  ;;         ;; otherwise splice in the quoted body
	  ;;         ``(the ,',ret-type
	  ;;             (progn ,@',real-body)))
	  ;; no we havent specified a return type, just fall in
	  (arg-expander-fn-aux 0 names types elements gensyms body)))

(defun arg-expander-fn (names types elements forms)
  "Helper function for def-tuple-op. Expands the arguments into a series of WITH-* forms so that
   symbols are bound to tuple elements in the body of the operator."
  (assert (= (length names) (length types) (length elements)) ()
		  "Malformed def-tuple-op argument list.")
  ;; if the first of the forms is a string then it's a docstring
  (let ((body (if (stringp (first forms)) (rest forms) forms)))
	;; create a gensym for every tuple element - they are going to be symbol macros
	(let ((gensyms
		   (mapcar #'(lambda (element-list)
					   (gensym-list (length element-list))) elements)))
	  ;; epand the body
	  (body-expander-fn names types elements gensyms body))))

										; tester
;; (arg-expander-fn '(v q) '(vector3d quaternion) '((x y z) (qx qy qz qw)) '("Return the vector + real" (:return (values single-float single-float single-float single-float) (vertex3d-tuple x y z qw))))
;; (arg-expander-fn '(v q n) '(vector3d quaternion single-float) '((x y z) (qx qy qz qw) nil) '("Return the vector + real" (:return vertex3d (vertex3d-tuple x y z qw))))
