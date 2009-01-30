;;;; symbols.lisp

(in-package :cl-tuples)

;; package used to hold tuple type info
(defpackage :tuple-types)

;; to do -- investigate cl-syntax-sugar to see if we can come up with
;; some nicer custom syntax

;; make #{ .. } notation become a short hand for (values ...)
(defun |#{-reader| (stream char arg)
  (declare (ignore char arg))
  `(values ,@(read-delimited-list #\} stream t)))

(set-dispatch-macro-character #\# #\{ #'|#{-reader|)
(set-macro-character #\} (get-macro-character #\) nil))

(defun make-tuple-symbol (type-name tuple-element-type tuple-initial-element elements)
  "Makes a symbol used to identify a typle type and interns it in the
package used for holding metadata about the tuple types. Information
about the tuple type is stored in the property list of the symbol."
  (assert (listp elements))
  (let
      ((type-name-sym (intern (string  type-name) :tuple-types)))
    (progn
      ;; deqfine the symbol
      ;; store-value the elements
      (setf (get type-name-sym 'elements) elements)
      ;; store the # of elements ( a bit redundant )
      (setf (get  type-name-sym 'tuple-length) (length elements))
	  ;; store-value to use as inital array element
	  (setf (get typename-sym 'initial-element) tuple-initial-element)
      ;; store-value the type of the elements
      (setf (get type-name-sym 'element-type) tuple-element-type)
      ;; store-value a flag us to make sure it's a tuple-type symbol
      (setf (get type-name-sym 'is-tuple) t))))


(defun tuple-typep (type-name)
  "Test to see if this symbol represents a tuple type"
  (when (or (symbolp type-name) (stringp type-name))
    (get (find-symbol (string-upcase (string type-name)) :tuple-types)  'is-tuple)))

(defun tuple-size (type-name)
  "Return the size of the type"
  (assert (or (symbolp type-name) (stringp type-name)))
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  'tuple-length))

(defun tuple-initial-element (type-name)
  "Return the inital element type of a tuple array"
  (assert (or (symbolp type-name) (stringp type-name)))
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  'initial-element)))

(defun tuple-element-type (type-name)
  "Return the size of the type"
  (assert (or (symbolp type-name) (stringp type-name)))
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  'element-type))

(defun tuple-elements (type-name)
  "Return a list of element names"
  (assert (or (symbolp type-name) (stringp type-name)))
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  'elements))

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

(defun tuple-places (type-name array-name)
  "Return a list of (aref *) forms to turn at tuple represeted and array into individual places."
  (loop 
     for i from 0 below (tuple-size type-name)
     collect `(aref ,array-name ,i)))
  