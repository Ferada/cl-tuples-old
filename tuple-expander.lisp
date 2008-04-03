
(in-package :cl-tuples)

(defparameter *tuple-expander-keywords* '(:def-tuple :def-tuple-getter :def-tuple-aref :def-with-tuple :def-with-tuple* :def-with-tuple-aref :def-tuple-setter :def-tuple-aref-setter :def-tuple-vector-push :def-tuple-vector-push-extend :def-new-tuple :def-tuple-maker :def-tuple-maker* :def-tuple-array-maker :def-tuple-array-dimensions :def-tuple-setf :def-tuple-array-setf :def-tuple-map :def-tuple-reduce))

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
  `(defmacro ,(tuple-symbol type-name expansion) (&rest values-form)
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
               `(vector-push-extend ,(nth index varlist) ,array-name))))))

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
                 :fill-pointer fill-pointer
                 :element-type ',(tuple-element-type type-name))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-array-dimensions)))
`(defun ,(tuple-symbol  type-name :def-tuple-array-dimensions) (tuple-array)
   (/ (car  (array-dimensions tuple-array)) ,(tuple-size type-name))))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-setf)))
  `(defsetf ,type-name ,(tuple-symbol type-name :def-tuple-setter)))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-array-setf)))
  `(defsetf ,(tuple-symbol type-name :def-tuple-aref)
     ,(tuple-symbol type-name :def-tuple-aref-setter)))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-map)))
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




