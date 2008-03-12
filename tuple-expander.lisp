
(in-package :cl-tuples)

;; to do - for each expansion, we want (make-expansion-name type-name expansion)

(defmethod tuple-expansion-fn (type-name expansion)
  (ccase expansion
    (:def-tuple
     `(defmacro ,(make-suffixed-symbol type-name "TUPLE") (&rest values-form)
        `(the (values ,@',(loop for i from 0 below (tuple-size type-name) collect (tuple-element-type type-name)))
           (values  ,@values-form))))
    (:def-tuple-getter
     `(defmacro ,type-name (tuple-array-name)
        `(values
          ,@(loop
               for index from 0 below (tuple-size ',type-name)
               collect
               `(aref ,tuple-array-name ,index)))))
    (:def-tuple-array-getter
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
    (:def-with-tuple
     `(defmacro ,(make-adorned-symbol type-name :prefix "WITH")  (tuple element-syms &body forms)
        `(multiple-value-bind
               ,element-syms
             ,tuple
           (declare (ignorable ,@element-syms))
           (progn ,@forms))))
    (:def-with-tuple*
     `(defmacro ,(make-adorned-symbol type-name :prefix "WITH" :asterisk t)  (tuple-array element-syms &body forms)
        `(multiple-value-bind
               ,element-syms
             (,',type-name ,tuple-array)
           (declare (ignorable ,@element-syms))
           (progn ,@forms))))
    (:def-with-tuple-aref
     `(defmacro ,(make-adorned-symbol type-name :prefix "WITH" :suffix "AREF")  ((tuple index element-syms) &body forms)
        `(symbol-macrolet
             ,(loop
                 for element-sym in element-syms
                 for array-index = (* index (tuple-size ',type-name))  then (1+ array-index)
                 collect `(,element-sym (aref ,tuple ,index)))
           (progn
             ,@forms))))
    (:def-tuple-setter
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
    (:def-tuple-aref-setter
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
    (:def-new-tuple
     `(defmacro ,(make-prefixed-symbol type-name "NEW") ()
        `(make-array (list ,',(tuple-size type-name)) :element-type ',',(tuple-element-type type-name))))
    (:def-tuple-maker
     `(defmacro ,(make-adorned-symbol type-name :prefix "MAKE") (tuple)
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
    (:def-tuple-maker*
     `(defmacro ,(make-adorned-symbol type-name :prefix "MAKE" :asterisk t) (&rest elements)
        (let ((tuple-sym (gensym)))
          `(let ((,tuple-sym (make-array (list ,',(tuple-size type-name)) :element-type ',',(tuple-element-type type-name))))
             (,',(make-suffixed-symbol type-name "SETTER") ,tuple-sym ,@elements)
             ,tuple-sym))))
    (:def-tuple-array-maker
     `(defun ,(make-adorned-symbol  type-name :prefix "MAKE" :suffix "ARRAY") (dimensions &key adjustable fill-pointer)
        (make-array (* ,(tuple-size type-name) dimensions)
                    :adjustable adjustable
                    :fill-pointer fill-pointer
                    :element-type ',(tuple-element-type type-name))))
    (:def-tuple-array-dimensions
     `(defun ,(make-adorned-symbol type-name :suffix "ARRAY-DIMENSIONS") (tuple-array)
        (/ (car  (array-dimensions tuple-array)) ,(tuple-size type-name))))
    (:def-tuple-setf
     `(defsetf ,type-name ,(make-suffixed-symbol type-name "SETTER")))
    (:def-tuple-array-setf
     `(defsetf ,(make-suffixed-symbol type-name "AREF")
          ,(make-suffixed-symbol type-name "AREF-SETTER")))
    (:def-tuple-map
     `(defmacro ,(make-adorned-symbol type-name :prefix "MAP" :suffix "TUPLES")  (fn &body tuples)
        `(macrolet
             ((map-values-aux (fn n gensym-lists &body v)
                  (if v
                      (let* ((gensym-list-sym (gensym-list n)))
                        `(multiple-value-bind
                               ,gensym-list-sym
                             ,(car v)
                           (declare (type ,(tuple-element-type ',type-name) ,gensym-list-sym))
                           (map-values-aux ,fn ,n (,@gensym-lists ,gensym-list-sym) ,@(cdr v))))
                      `(values
                        ,@(loop
                             for index from 0 below (length (car gensym-lists))
                             collect
                             `(funcall ,fn
                                       ,@(loop
                                            for gensym-list in gensym-lists
                                            collect (nth index gensym-list))))))))
           (map-values-aux ,fn ,(tuple-size ',type-name)  NIL ,@tuples))))
    (:def-tuple-reduce
     `(defmacro ,(make-adorned-symbol type-name :prefix "REDUCE" :suffix "TUPLE") (fn  tuples)
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
           (reduce-values ,fn  ,(tuple-size ',type-name)  ,tuples))))))
