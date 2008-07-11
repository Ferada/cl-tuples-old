
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
  (destructuring-bind (name &key (type nil) (readonly nil) (allocation nil))   
      slot-spec 
    (declare (ignore type))
    `(,name :initarg ,(as-keyword name) 
            ,@(if readonly
                  (list :reader (as-private-accessor class-name name))
                  (list :accessor (as-private-accessor class-name name)))
            ,@(if allocation
                  (list :allocation allocation)))))

(defun slot->slot-accessor (class-name slot-spec)
  (destructuring-bind (name &key (type nil) (readonly nil) (allocation nil))
      slot-spec 
    (declare (ignore allocation))
    (progn
        (progn
          `(defmethod ,(as-accessor name) ((self ,class-name))
             (,(tuple-symbol :def-tuple type) (,(as-private-accessor class-name name) self))))
        (unless readonly
          ;; define a setf defmethod
          (progn
            `(defsetf  ,(as-accessor name) (object) ,(tuple-gensyms type)
                 (setf (,(tuple-symbol type :def-tuple-getter)  (,(as-private-accessor class-name name) object) ,(tuple-gensyms type)))))))))


(defmacro define-tuple-class (name slots)
  "Define a class that contains only tuple slots."
    `(progn 
       (defclass ,name ()
         ,(mapcar #'(lambda (slot-spec) 
                      (slot->defclass-slot name slot-spec)) slots)
         ,(mapcar 
           #'(lambda (slot-spec)
               (slot->slot-accessor name slot-spec)) slots))))
