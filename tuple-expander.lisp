
(in-package :cl-tuples)

;; to do - for each expansion, we want (make-expansion-name type-name expansion)

(defmethod tuple-symbol (type-name expansion)
  "Given the expansion, return the name of the macro/function associated with it."
  (ccase expansion
    (:def-tuple (make-suffixed-symbol type-name "TUPLE"))
    (:def-tuple-getter (make-adorned-symbol type-name))    
    (:def-tuple-aref (make-suffixed-symbol type-name "AREF"))
    (:def-with-tuple (make-adorned-symbol type-name :prefix "WITH"))
    (:def-with-tuple* (make-adorned-symbol type-name :prefix "WITH" :asterisk t))
    (:def-with-tuple-aref (make-adorned-symbol type-name :prefix "WITH" :suffix "AREF"))
    (:def-tuple-setter (make-suffixed-symbol type-name "SETTER"))
    (:def-tuple-aref-setter (make-suffixed-symbol type-name "AREF-SETTER") )
    (:def-tuple-vector-push (make-adorned-symbol type-name :suffix "VECTOR-PUSH"))
    (:def-tuple-vector-push-extend (make-adorned-symbol type-name :suffix "VECTOR-PUSH-EXTEND"))  
    (:def-new-tuple (make-prefixed-symbol type-name "NEW"))
    (:def-tuple-maker (make-adorned-symbol type-name :prefix "MAKE"))
    (:def-tuple-maker* (make-adorned-symbol type-name :prefix "MAKE" :asterisk t))
    (:def-tuple-array-maker (make-adorned-symbol type-name :prefix "MAKE" :suffix "ARRAY"))
    (:def-tuple-array-dimensions (make-adorned-symbol type-name :suffix "ARRAY-DIMENSIONS"))
    (:def-tuple-setf (make-suffixed-symbol type-name "SETTER"))
    (:def-tuple-array-setf (make-suffixed-symbol type-name "AREF"))
    (:def-tuple-map (make-adorned-symbol type-name :prefix "MAP" :suffix "TUPLES"))
    (:def-tuple-reduce (make-adorned-symbol type-name :prefix "REDUCE" :suffix "TUPLE"))))

;; to do -- break this up into methods specialised by key
(defmethod tuple-expansion-fn (type-name expansion)
  (ccase expansion
    (:def-tuple
     `(defmacro ,(tuple-symbol type-name expansion) (&rest values-form)
        `(the (values ,@',(loop for i from 0 below (tuple-size type-name) collect (tuple-element-type type-name)))
           (values  ,@values-form))))
    (:def-tuple-getter
     `(defmacro ,type-name (tuple-array-name)
        `(the (values ,@',(loop for i from 0 below (tuple-size type-name) collect (tuple-element-type type-name)))
           (values
            ,@(loop
                 for index from 0 below (tuple-size ',type-name)
                 collect
                   `(aref ,tuple-array-name ,index))))))
    (:def-tuple-aref
     `(defmacro ,(tuple-symbol type-name :def-tuple-aref) (target-sym array-index)
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
     `(defmacro ,(tuple-symbol type-name :def-with-tuple)  (tuple element-syms &body forms)
        `(multiple-value-bind
               ,element-syms
             ,tuple
           (declare (ignorable ,@element-syms))
           (progn ,@forms))))
    (:def-with-tuple*
     `(defmacro ,(tuple-symbol type-name :def-with-tuple*)  (tuple-array element-syms &body forms)
        `(multiple-value-bind
               ,element-syms
             (,',type-name ,tuple-array)
           (declare (ignorable ,@element-syms))
           (progn ,@forms))))
    (:def-with-tuple-aref
     `(defmacro ,(tuple-symbol type-name :def-with-tuple-aref)  ((array-name index element-syms) &body forms)
        `(symbol-macrolet
             ,(loop
                 for element-sym in element-syms
                 for array-index = (* index (tuple-size ',type-name))  then (1+ array-index)
                 collect `(,element-sym (aref ,array-name ,index)))
           (progn
             ,@forms))))
    (:def-tuple-setter
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
    (:def-tuple-aref-setter
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
    (:def-tuple-vector-push
     `(defmacro ,(tuple-symbol type-name :def-tuple-vector-push) (tuple-values array-name)
        (let* ((varlist (gensym-list ,(tuple-size type-name))))
          `(multiple-value-bind
                 ,varlist
               ,tuple-values
             (loop 
                  for index from 0 below ,(tuple-size type-name)
                  collect
                  `(vector-push ,(nth index varlist) ,array-name))))))
    (:def-tuple-vector-push-extend
     `(defmacro ,(tuple-symbol type-name :def-tuple-vector-push-extend) (tuple-values array-name)
        (let* ((varlist (gensym-list ,(tuple-size type-name))))
          `(multiple-value-bind
                 ,varlist
               ,tuple-values
             (loop 
                for index from 0 below ,(tuple-size type-name)
                collect
                  `(vector-push-extend ,(nth index varlist) ,array-name))))))
    (:def-new-tuple
     `(defmacro ,(tuple-symbol type-name :def-new-tuple) ()
        `(make-array (list ,',(tuple-size type-name)) :element-type ',',(tuple-element-type type-name))))
    (:def-tuple-maker
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
    (:def-tuple-maker*
     `(defmacro ,(tuple-symbol type-name :def-tuple-maker*) (&rest elements)
        (let ((tuple-sym (gensym)))
          `(let ((,tuple-sym (make-array (list ,',(tuple-size type-name)) :element-type ',',(tuple-element-type type-name))))
             (,',(tuple-symbol type-name :def-tuple-setter) ,tuple-sym ,@elements)
             ,tuple-sym))))
    (:def-tuple-array-maker
     `(defun ,(tuple-symbol type-name :def-tuple-array-maker) (dimensions &key adjustable fill-pointer)
        (make-array (* ,(tuple-size type-name) dimensions)
                    :adjustable adjustable
                    :fill-pointer fill-pointer
                    :element-type ',(tuple-element-type type-name))))
    (:def-tuple-array-dimensions
     `(defun ,(tuple-symbol  type-name :def-tuple-array-dimensions) (tuple-array)
        (/ (car  (array-dimensions tuple-array)) ,(tuple-size type-name))))
    (:def-tuple-setf
     `(defsetf ,type-name ,(tuple-symbol type-name :def-tuple-setter)))
    (:def-tuple-array-setf
     `(defsetf ,(tuple-symbol type-name :def-tuple-aref)
          ,(tuple-symbol type-name :def-tuple-aref-setter)))
    (:def-tuple-map
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
           (map-values-aux ,fn ,(tuple-size ',type-name)  NIL ,@tuples))))
    (:def-tuple-reduce
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
           (reduce-values ,fn  ,(tuple-size ',type-name)  ,tuples))))))



;; (defun map-binding-expansion-fn (type-name)
;;   `(defmacro ,(make-adorned-symbol type-name :prefix "MAP" :suffix "TUPLES") (fn-sym &body value-lists)
;;      (let* ((n ,(tuple-size type-name))
;;             (gensym-lists (loop for i from 0 below (length value-lists)
;;                              collect
;;                              (gensym-list n))))
;;        (labels ((map-value-expansion-fn (symbol-lists)
;;                   `(values
;;                     ,@(loop for i from 0 below n
;;                          collect `(funcall ,fn-sym
;;                                            ,@(loop 
;;                                                 for lst in symbol-lists
;;                                                 collect (nth i lst))))))
;;                 (expand-car (value-lists sym-lists)
;;                   `(multiple-value-bind  ,(car sym-lists) ,(car value-lists)
;;                      ,(if (cdr value-lists)
;;                           (expand-car (cdr value-lists) (cdr sym-lists))
;;                           (map-value-expansion-fn gensym-lists)))))
;;          (expand-car value-lists gensym-lists)))))

