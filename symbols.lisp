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

(defun make-tuple-symbol (type-name tuple-element-type elements)
  "Makes a symbol used to identify a typle type. Information about the tuple type
is stored in the property list of the symbol."
  (assert (listp elements))
  (let
      ((type-name-sym (intern (string  type-name) :tuple-types)))
    (progn
      ;; deqfine the symbol
      ;; store-value the elements
      (setf (get type-name-sym 'elements) elements)
      ;; store the # of elements ( a bit redundant )
      (setf (get  type-name-sym 'tuple-length) (length elements))
      ;; store-value the type of the elements
      (setf (get type-name-sym 'element-type) tuple-element-type)
      ;; store-value a flag us to make sure it's a tuple-type symbol
      (setf (get type-name-sym 'is-tuple) t))))


(defun tuple-typep (type-name)
  "Test to see if this symbol represents a tuple type"
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  'is-tuple))

(defun tuple-size (type-name)
  "Return the size of the type"
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  'tuple-length))


(defun tuple-element-type (type-name)
  "Return the size of the type"
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  'element-type))

(defun tuple-elements (type-name)
  "Return the size of the type"
  (get (find-symbol (string-upcase (string type-name)) :tuple-types)  'elements))

(defun tuple-typespec (type-name)
  "Return typespec of tuple."
  `(values ,@(loop 
               for i from 0 to (tuple-size type-name) 
                collect (tuple-element-type type-name))))

