
(in-package :cl-tuples)

(defparameter *tuple-expander-keywords* 
  '(:def-tuple :def-tuple-getter :def-tuple-aref 
    :def-with-tuple :def-with-tuple* :def-with-tuple-aref 
    :def-tuple-setter :def-tuple-aref-setter 
    :def-tuple-vector-push :def-tuple-vector-push-extend 
    :def-new-tuple :def-tuple-maker :def-tuple-maker* 
    :def-tuple-array-maker :def-tuple-array-dimensions 
    :def-tuple-setf :def-tuple-array-setf 
    :def-tuple-map :def-tuple-reduce))

(defgeneric tuple-symbol (type-name expansion))
;;  "Given the expansion, return the name of the macro/function associated with it."

(defmethod tuple-symbol ((type-name string) expansion)
  (tuple-symbol (find-symbol type-name) expansion))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple)))
  (make-suffixed-symbol type-name "TUPLE"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-getter))) 
  (make-adorned-symbol type-name))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-aref)))    
  (make-suffixed-symbol type-name "AREF"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-with-tuple)))    
  (make-adorned-symbol type-name :prefix "WITH"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-with-tuple*)))    
  (make-adorned-symbol type-name :prefix "WITH" :asterisk t))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-with-tuple-aref)))    
  (make-adorned-symbol type-name :prefix "WITH" :suffix "AREF"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-setter)))    
  (make-suffixed-symbol type-name "SETTER"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-aref-setter)))    
  (make-suffixed-symbol type-name "AREF-SETTER"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-vector-push)))    
  (make-adorned-symbol type-name :suffix "VECTOR-PUSH"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-vector-push-extend)))    
  (make-adorned-symbol type-name :suffix "VECTOR-PUSH-EXTEND"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-new-tuple)))    
  (make-prefixed-symbol type-name "NEW"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-maker)))    
  (make-adorned-symbol type-name :prefix "MAKE"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-maker*)))    
  (make-adorned-symbol type-name :prefix "MAKE" :asterisk t))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-array-maker)))    
  (make-adorned-symbol type-name :prefix "MAKE" :suffix "ARRAY"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-array-dimensions)))    
  (make-adorned-symbol type-name :suffix "ARRAY-DIMENSIONS"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-setf)))    
  (make-suffixed-symbol type-name "SETTER"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-array-setf)))    
  (make-suffixed-symbol type-name "AREF"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-map)))    
  (make-adorned-symbol type-name :prefix "MAP" :suffix "TUPLES"))

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-reduce)))    
  (make-adorned-symbol type-name :prefix "REDUCE" :suffix "TUPLE"))


;; to do -- break this up into methods specialised by key
(defgeneric tuple-expansion-fn (type-name expansion))


(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple)))
  `(defmacro ,(tuple-symbol type-name expansion) (&rest elements)
     `(the (values ,@',(loop for i from 0 below (tuple-size type-name) collect (tuple-element-type type-name)))
        (values  ,@values-form))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-getter)))
  `(defmacro ,type-name (tuple-array-name)
  `(the (values ,@',(loop for i from 0 below (tuple-size type-name) collect (tuple-element-type type-name)))
     (values
      ,@(loop
         for index from 0 below (tuple-size ',type-name)
         collect
         `(aref ,tuple-array-name ,index))))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-aref)))
  `(defmacro ,(tuple-symbol type-name :def-tuple-aref) (tuple-array array-index)
     (let* ((varlist (gensym-list ,(tuple-size type-name)))
            (array-index-sym (gensym))
            (counter-sym (gensym)))
       `(let ((,array-index-sym (* ,',(tuple-size type-name) ,array-index))
              (,counter-sym 0))
          (values ,@(mapcar #'(lambda (x)
                  (declare ( ignore x))
                  (list
                   'prog1
                   `(aref ,tuple-array
                          (+ ,counter-sym ,array-index-sym))
                   `(incf ,counter-sym)))
                            varlist))))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-with-tuple)))
  `(defmacro ,(tuple-symbol type-name :def-with-tuple)  (tuple element-syms &body forms)
     `(multiple-value-bind
          ,element-syms
          ,tuple
        (declare (ignorable ,@element-syms))
        (progn ,@forms))))


(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql  :def-with-tuple*)))
  `(defmacro ,(tuple-symbol type-name :def-with-tuple*) (tuple-array element-syms &body forms)
     `(multiple-value-bind
          ,element-syms
          (,',type-name ,tuple-array)
        (declare (ignorable ,@element-syms))
        (progn ,@forms))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-with-tuple-aref)))
  `(defmacro ,(tuple-symbol type-name :def-with-tuple-aref)  ((array-name index element-syms) &body forms)
     `(symbol-macrolet
          ,(loop
            for element-sym in element-syms
            for array-index = (* index (tuple-size ',type-name))  then (1+ array-index)
            collect `(,element-sym (aref ,array-name ,index)))
        (progn
          ,@forms))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-setter)))
  `(defmacro ,(tuple-symbol type-name :def-tuple-setter) (tuple-place tuple-values)
     (let* ((varlist (gensym-list ,(tuple-size type-name))))
       `(multiple-value-bind
            ,varlist
            ,tuple-values
          (values
           ,@(loop
                for index from 0 below ,(tuple-size type-name)
                collect
                  `(setf (aref ,tuple-place ,index) ,(nth index varlist))))))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-aref-setter)))
  `(defmacro ,(tuple-symbol type-name :def-tuple-aref-setter) (array-name array-index tuple)
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
                                   `(setf (aref ,array-name
                                                (+ ,counter-sym ,array-index-sym)) ,x)
                                   `(incf ,counter-sym)))
                              varlist)))))))

;; tuple-vector-push
(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-vector-push)))
  `(defmacro ,(tuple-symbol type-name :def-tuple-vector-push) (tuple-values array-name)
     (let* ((varlist (gensym-list ,(tuple-size type-name))))
       `(multiple-value-bind
            ,varlist
            ,tuple-values
          ,@(loop
              for index from 0 below ,(tuple-size type-name)
              collect
                `(vector-push ,(nth index varlist) ,array-name))))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-vector-push-extend)))
  `(defmacro ,(tuple-symbol type-name :def-tuple-vector-push-extend) (tuple-values array-name)
     (let* ((varlist (gensym-list ,(tuple-size type-name))))
       `(multiple-value-bind
            ,varlist
            ,tuple-values
          ,@(loop
             for index from 0 below ,(tuple-size type-name)
             collect
               `(vector-push-extend ,(nth index varlist) ,array-name ,',(tuple-size type-name)))))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-new-tuple)))
  `(defmacro ,(tuple-symbol type-name :def-new-tuple) ()
     `(make-array (list ,',(tuple-size type-name)) :element-type ',',(tuple-element-type type-name))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-maker)))
  `(defmacro ,(tuple-symbol type-name :def-tuple-maker) (tuple)
     (let ((varlist (gensym-list ,(tuple-size type-name)))
           (tuple-sym (gensym))
           (counter-sym 0))
  `(let  ((,tuple-sym (make-array (list ,',(tuple-size type-name)) :element-type ',',(tuple-element-type type-name))))
     (multiple-value-bind
         ,varlist
         ,tuple
       (progn ,@(mapcar #'(lambda (x)
                            (prog1
                                `(setf (aref ,tuple-sym ,counter-sym) ,x)
                              (incf counter-sym)))
                        varlist)
              ,tuple-sym))))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-maker*)))
  `(defmacro ,(tuple-symbol type-name :def-tuple-maker*) (&rest elements)
     (let ((tuple-sym (gensym)))
       `(let ((,tuple-sym (make-array (list ,',(tuple-size type-name)) :element-type ',',(tuple-element-type type-name))))
          (,',(tuple-symbol type-name :def-tuple-setter) ,tuple-sym (values  ,@elements))
          ,tuple-sym))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-array-maker)))
  `(defun ,(tuple-symbol type-name :def-tuple-array-maker) (dimensions &key adjustable fill-pointer)
     (make-array (* ,(tuple-size type-name) dimensions)
                 :adjustable adjustable
                 :fill-pointer (* ,(tuple-size type-name) fill-pointer)
                 :element-type ',(tuple-element-type type-name))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-array-dimensions)))
`(defun ,(tuple-symbol  type-name :def-tuple-array-dimensions) (tuple-array)
   (/ (length tuple-array) ,(tuple-size type-name))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-setf)))
  "Expand form that creates generalized reference to tuples"  
  `(defsetf ,type-name ,(tuple-symbol type-name :def-tuple-setter)))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-array-setf)))
  "Expand form that creates generalized reference to tuple-arrays"
  `(defsetf ,(tuple-symbol type-name :def-tuple-aref)
     ,(tuple-symbol type-name :def-tuple-aref-setter)))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-map)))
  "Expand def-tuple-map form"
  `(defmacro ,(tuple-symbol type-name :def-tuple-map)  (fn &body tuples)
     `(macrolet
          ((map-values-aux (fn n gensym-lists &body v)
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
                                               collect (nth index gensym-list))))))))
        (map-values-aux ,fn ,(tuple-size ',type-name) nil ,@tuples))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-reduce)))
  "Expand def-tuple-reduce form"
  `(defmacro ,(tuple-symbol type-name :def-tuple-reduce) (fn  tuples)
     `(macrolet
          ((reduce-values-body (fn n syms v)
                               (cond
                                ((= (length syms) 1)
                                 (list (car syms)))
                                ((= (length syms) 2)
                                 `(funcall ,fn ,(car syms) ,(cadr syms)))
                                ((> (length syms) 2)
                                 `(funcall ,fn ,(car syms) (reduce-values-body ,fn ,n ,(cdr syms) ,v)))))
           (reduce-values (fn n v)
                          (let*
                              ((gensym-list-sym (gensym-list n)))
                            `(multiple-value-bind
                                 ,gensym-list-sym
                                 ,v
                               (reduce-values-body ,fn ,n ,gensym-list-sym ,v)))))
        (reduce-values ,fn  ,(tuple-size ',type-name)  ,tuples))))


;; -- def-tuple-op expanders begin here ------------------------------------

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
          (symbol-macrolet ((,',(nth n names) (,',(make-adorned-symbol (nth n types) :suffix "TUPLE")
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
;; 
