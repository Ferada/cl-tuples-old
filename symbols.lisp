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
                          (tuple-typespec (cadar body))
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
;; (arg-expander-fn '(v q) '(vector3d quaternion) '((x y z) (qx qy qz qw)) '("Return the vector + real" (:return vertex3d (vertex3d-tuple x y z qw))))
