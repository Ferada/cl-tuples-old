(in-package :cl-tuples)

(defun as-keyword (sym)
  (intern (string sym) :keyword))

(defun mklist (x)
  "Return the argument as a list if it isn't already"
  (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  "Return the slot as a list of lists"
  (list (first spec) (mklist (cdr spec))))

(defun as-private-accessor (class-sym slot-sym)
  "Create an accessor for the slot named by sym"
  (intern (concatenate 'string (string class-sym) "-" (string slot-sym) "%")))

(defun as-accessor (sym)
  "Create an accessor for the slot named by sym"
  (intern (concatenate 'string (string sym) "-OF")))


(defun slot->defclass-slot (class-name slot-spec)
  "Map a define-tuple-class slot spec to a clos slot spec"
  (destructuring-bind (name &key (type nil) (array nil) (readonly nil) (allocation nil))
      slot-spec
    (if array
        `(,name :initarg ,(as-keyword name)
                :type ,(tuple-typespec** type)
                :initform (,(tuple-symbol type :def-tuple-array-maker) ,array)
                ,@(if readonly
                      (list :reader (as-private-accessor class-name name))
                      (list :accessor (as-private-accessor class-name name)))
                ,@(if allocation
                      (list :allocation allocation)))
        `(,name :initarg ,(as-keyword name)
                :type ,(tuple-typespec* type)
                :initform (,(tuple-symbol type :def-new-tuple))
                ,@(if readonly
                      (list :reader (as-private-accessor class-name name))
                      (list :accessor (as-private-accessor class-name name)))
                ,@(if allocation
                      (list :allocation allocation))))))

(defun slot->slot-reader (class-name slot-spec)
  "Expand a generic method body for reading a tuple slot of a def-tuples-class form"
  (destructuring-bind (name &key (type nil) (array nil) (readonly nil) (allocation nil))
      slot-spec
    (declare (ignore allocation readonly))
    (if array
        ;; array form
        `(defmethod ,(as-accessor name) ((self ,class-name) index)
           (the ,(tuple-typespec type)
             (,(tuple-symbol type  :def-tuple-aref) 
               (the ,(tuple-typespec** type) (,(as-private-accessor class-name name) self)) (the fixnum index))))
        ;; scalar form
        `(defmethod ,(as-accessor name) ((self ,class-name))
           (the ,(tuple-typespec type)
             (,(tuple-symbol type  :def-tuple-getter) (the ,(tuple-typespec* type) (,(as-private-accessor class-name name) self))))))))

(defun slot->slot-writer (class-name slot-spec)
  "Expand a generic method body for writing a tuple slot of a def-tuples-class form"
  (destructuring-bind (name &key (type nil)  (array nil) (readonly nil) (allocation nil))
      slot-spec
    (declare (ignore allocation))
    (unless readonly
      (if array
          ;; array form
          (progn
            (let* ((tuple-gensym-list (tuple-gensyms type))
                   (annotated-tuple-gensym-list (mapcar #'(lambda (x) ``(the ,',(tuple-element-type type)  ,,x)) tuple-gensym-list)))
              `(defsetf  ,(as-accessor name) (object index)  ,tuple-gensym-list
                 `(setf (,',(tuple-symbol type :def-tuple-aref)
                            (the ,',(tuple-typespec** type) (,',(as-private-accessor class-name name) (the ,',class-name ,object))) 
                            (the fixnum ,index))
                        (the ,',(tuple-typespec type)
                          (,',(tuple-symbol type :def-tuple)
                              ,,@annotated-tuple-gensym-list))))))
          ;; scalar form
          (progn
            (let* ((tuple-gensym-list (tuple-gensyms type))
                   (annotated-tuple-gensym-list (mapcar #'(lambda (x) ``(the ,',(tuple-element-type type)  ,,x)) tuple-gensym-list)))
              `(defsetf  ,(as-accessor name) (object)  ,tuple-gensym-list
                 `(setf (,',(tuple-symbol type :def-tuple-getter)
                            (the ,',(tuple-typespec* type) (,',(as-private-accessor class-name name) (the ,',class-name ,object))))
                        (the ,',(tuple-typespec type)
                                 (,',(tuple-symbol type :def-tuple)
                                     ,,@annotated-tuple-gensym-list))))))))))


