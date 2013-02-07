;;;; symbols.lisp

(in-package :cl-tuples)

;; package used to hold tuple type info
(defpackage :tuple-types)

(defun make-tuple-symbol (type-name tuple-element-type tuple-initial-element elements)
  "Makes a symbol used to identify a tuple type and interns it in the
package used for holding metadata about the tuple types. Information
about the tuple type is stored in the property list of the symbol."
  (assert (listp elements))
  (let*	  
      ((type-string (string-upcase (string  type-name)))
	   (type-name-sym (intern  type-string :tuple-types))
	   (value-name-sym (intern (concatenate 'string type-string "*") :tuple-types)))
    (progn
      ;; deqfine the symbol
      ;; store-value the elements
      (setf (get type-name-sym :elements) elements)
      (setf (get value-name-sym :elements) elements)
      ;; store the # of elements ( a bit redundant )
      (setf (get  type-name-sym :tuple-length) (length elements))
      (setf (get  value-name-sym :tuple-length) (length elements))
	  ;; store-value to use as inital array element
	  (setf (get type-name-sym :initial-element) tuple-initial-element)
	  (setf (get value-name-sym :initial-element) tuple-initial-element)
      ;; store-value the type of the elements
      (setf (get type-name-sym :element-type) tuple-element-type)
      (setf (get value-name-sym :element-type) tuple-element-type)
      ;; store-value a flag us to make sure it's a tuple-type symbol
      (setf (get type-name-sym :is-tuple) t)
      (setf (get value-name-sym :is-tuple) t))))


(defun tuple-typep (type-name)
  "Test to see if this symbol represents a tuple type"
  (when (or (symbolp type-name) (stringp type-name))
    (get (find-symbol (string-upcase (string type-name)) :tuple-types)  :is-tuple)))

(defun tuple-size (type-name)
  "Return the size of the type"
  (assert (or (symbolp type-name) (stringp type-name)))
  (the  fixnum
		(get (find-symbol (string-upcase (string type-name)) :tuple-types)  :tuple-length)))

(defun tuple-initial-element (type-name)
  "Return the inital element type of a tuple array"
  (assert (or (symbolp type-name) (stringp type-name)))
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  :initial-element))

(defun tuple-element-type (type-name)
  "Return the size of the type"
  (assert (or (symbolp type-name) (stringp type-name)))
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  :element-type))

(defun tuple-elements (type-name)
  "Return a list of element names"
  (assert (or (symbolp type-name) (stringp type-name)))
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  :elements))

(defun tuple-gensyms (type-name)
  "Return a list of gensyms, one for each element of the tuple"
  (assert (or (symbolp type-name) (stringp type-name)))
  (loop 
     for i from 0 below (tuple-size type-name)
     collect (gensym)))

(defun tuple-typespec (type-name)
  "Return typespec of tuple as multiple value."
  `(values ,@(loop 
               for i from 0 below (tuple-size type-name) 
                collect (tuple-element-type type-name))))

(defun tuple-typespec* (type-name)
  "Return typespec of tuple as bounded array"
  `(vector ,(tuple-element-type type-name) ,(tuple-size type-name)))

(defun tuple-typespec** (type-name)
  "Return typespec of tuple as unbounded array"
  `(vector ,(tuple-element-type type-name) *))

(defun simple-tuple-typespec* (type-name)
  "Return typespec of tuple as bounded array"
  `(simple-vector  ,(tuple-size type-name)))

(defun tuple-places (type-name array-name)
  "Return a list of (aref *) forms to turn at tuple represeted and array into individual places."
  (loop 
     for i from 0 below (tuple-size type-name)
     collect `(aref ,array-name ,i)))
  
;; make #{ .. } notation become a short hand for (values ...)
(defun |#{-reader| (stream char arg)
  (declare (ignore char arg))
  `(values ,@(read-delimited-list #\} stream t)))

(set-dispatch-macro-character #\# #\{ #'|#{-reader|)
(set-macro-character #\} (get-macro-character #\) nil))

(defun |#[-reader| (stream char arg)
  (declare (ignore char arg))
  (let ((form (read-delimited-list #\] stream t)))
	(if (tuple-typep (car form))
		(if (is-asterisk-symbol (car form))		
			(let* ((form-str (symbol-name (car form)))
				   (tuple-str (subseq form-str 0 (- (length form-str) 1))))
			  `(,(make-adorned-symbol tuple-str :asterisk t :suffix "VALUES") ,@(cdr form)))
			`(,(make-adorned-symbol (car form) :prefix "MAKE") ,@(cdr form)))		
		(error "~A does not define a tuple type" (car form)))))
			
		
		

(set-dispatch-macro-character #\# #\[ #'|#[-reader|)
(set-macro-character #\] (get-macro-character #\) nil))
