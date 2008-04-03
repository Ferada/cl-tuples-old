;;;; symbols.lisp

(in-package :cl-tuples)

;; package used to hold tuple type info
(defpackage :tuple-types)

;; make #{ .. } notation become a short hand for (values ...)
(defun |#{-reader| (stream char arg)
  (declare (ignore char arg))
  `(values ,@(read-delimited-list #\} stream t)))

(set-dispatch-macro-character #\# #\{ #'|#{-reader|)
(set-macro-character #\} (get-macro-character #\) nil))

(defmacro with-gensyms ((&rest names) &body body)
  "Classic macro for creating named unique symbols."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  "Evaluate arguments once only in macro form body"
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))

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


(defun make-adorned-symbol (name &key prefix suffix asterisk package)
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
                         (string "*")))
          (if package package *package*)))

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

(defun tuple-typespec (type-name)
  "Return typespec of tuple."
  `(values ,@(loop 
               for i from 0 to (tuple-size type-name) 
                collect (tuple-element-type type-name))))

(defun symbol-macro-expander-fn (n names types elements gensyms body)
  "Wrap the body of def tuple op in symbol macros mapped to gensyms to prevent
   name capture."
  (if (nth n elements)
    ``(symbol-macrolet 
          ,',(loop 
                for gensym in (nth n gensyms) 
                for element in (nth n elements) collect `(,element  ,gensym))
        (declare (ignorable ,@',(nth n gensyms)))
        (symbol-macrolet ((,',(nth n names) (,',(make-adorned-symbol (nth n types) :suffix "TUPLE")
                                                ,@',(loop
                                                       for gensym in (nth n gensyms)
                                                       collect gensym))))                         
                         ,,(if (< (1+ n) (length names))
                               (symbol-macro-expander-fn (1+ n) names types elements gensyms body)
                               ``(progn ,@',body))))
    (if (< (1+ n) (length names))
        (symbol-macro-expander-fn (1+ n) names types elements gensyms body)
        ``(progn ,@',body))))
                                      

(defun arg-expander-fn-aux (n names types elements gensyms body)
  (if (nth n types)
      ``(,',(make-adorned-symbol (nth n types) :prefix "WITH")
          ,,(nth n  names) ,',(nth n  gensyms)
          ,,(if (< (1+ n) (length names))
                (arg-expander-fn-aux (1+ n) names types elements gensyms body)
                (symbol-macro-expander-fn 0 names types elements gensyms body)))
      ``(symbol-macrolet ((,',(nth n names) ,,(nth n names)))
          ,,(if (< (1+ n) (length names))
                (arg-expander-fn-aux (1+ n) names types elements gensyms body)
                (symbol-macro-expander-fn 0 names types elements gensyms body)))))


(defun body-expander-fn (names types elements gensyms body)
  (if (eq (caar body) :return)
      (let ((ret-type (if (tuple-typep (cadar body))
                          
                          (cadar body)))
          (real-body (cddar body)))
      `(the ,ret-type
         ,(arg-expander-fn-aux 0 names types elements gensyms real-body)))
    (arg-expander-fn-aux 0 names types elements gensyms body)))

(defun arg-expander-fn (names types elements forms)
  "Helper function for def-tuple-op. Expands the arguments into a series of WITH-* forms so that
   symbols are bound to tuple elements in the body of the operator."
  (assert (= (length names) (length types) (length elements)) ()
          "Malformed def-tuple-op argument list.")
  (let ((body (if (stringp (first forms)) (rest forms) forms)))
    (if (car types)
        (let ((gensyms 
               (mapcar #'(lambda (element-list) 
                           (gensym-list (length element-list))) elements)))
          (body-expander-fn names types elements gensyms body)))))

; tester
;; (arg-expander-fn '(v q) '(vector3d quaternion) '((x y z) (qx qy qz qw)) '("Return the vector + real" (:return (values single-float single-float single-float single-float) (vertex3d-tuple x y z qw))))
;; (arg-expander-fn '(v q) '(vector3d quaternion) '((x y z) (qx qy qz qw)) '("Return the vector + real" ((:return (values single-float single-float single-float single-float)) (vertex3d-tuple x y z qw))))
