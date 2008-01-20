;;;; symbols.lisp

(in-package :cl-tuples)


;; package used to hold tuple type info
(defpackage :tuple-types)

;; make #{ .. } notation become a short hand for (values ...)
(defun |#{-reader| (stream char arg)
  (declare (ignore char arg))
  `(values ,@(read-delimited-list \}|# stream t)))

(set-dispatch-macro-character #\# #\{ #'|#{-reader|)
(set-macro-character #\} (get-macro-character #\) nil))

;; define helper functions we will use

(defun gensym-list (n)
  "Give us a list of gensyms n elements long"
  (loop
     for index from 0 below n
     collect (gensym)))


(defun symbol-to-string (x)
  "If the argument is a symbol or string, return it as a string."
  (check-type x (or symbol string))
  (cond
    ((symbolp x)
     (symbol-name x))
    ((stringp x)
     x)))

(defun make-adorned-symbol (name &key prefix suffix asterisk)
  (check-type name symbol)
  (check-type prefix (or symbol string null))
  (check-type suffix (or symbol string null))
  (intern (concatenate 'string
                       (when prefix
                         (string prefix))
                       (when prefix  "-")
                       (string name)
                       (when suffix
                         "-")
                       (when suffix
                         (string suffix))
                       (when asterisk
                         (string "*")))))

(defun make-suffixed-symbol (name suffix)
  (make-adorned-symbol name :suffix suffix))

(defun make-prefixed-symbol (name prefix)
  (make-adorned-symbol name :prefix prefix))

(defun make-element-names (elements type-name)
  "Given a list of element names form a set of symbols of the form
     <type-name>-<element-name> as used in struct elements."
  (check-type elements symbol)
  (check-type type-name symbol)
  (mapcar #'(lambda (x)
              (find-symbol
               (concatenate 'string
                            (symbol-name type-name) "-struct-"
                            (symbol-name x))))
          elements))


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


(defun arg-expander-fn (names types elements forms)
  "Helper function for def-tuple-op. Expands the arguments into a series of WITH-* forms so that
   symbols are bound to tuple elements in the body of the operator."
  (if (car types)
      ``(,',(make-adorned-symbol (car types) :prefix "WITH")
            ,,(car  names) ,',(car elements) 
            ,,(if (cdr names)
                  (arg-expander-fn (cdr names) (cdr types) (cdr elements) forms)
                  ``(progn ,@',forms)))
     (if (cdr names)
         (arg-expander-fn (cdr names) (cdr types) (cdr elements) forms)
         ``(progn ,@',@forms))))
