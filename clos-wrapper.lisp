
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
             (,(tuple-symbol type  :def-tuple-aref) (,(as-private-accessor class-name name) self) (the fixnum index))))
        ;; scalar form
        `(defmethod ,(as-accessor name) ((self ,class-name))
           (the ,(tuple-typespec type) 
             (,(tuple-symbol type  :def-tuple-getter) (,(as-private-accessor class-name name) self)))))))

(defun slot->slot-writer (class-name slot-spec)
  "Expand a generic method body for writing a tuple slot of a def-tuples-class form"
  (destructuring-bind (name &key (type nil)  (array nil) (readonly nil) (allocation nil))
      slot-spec 
    (declare (ignore allocation))
    (unless readonly
      (if array
          ;; array form
          (progn
            (let ((tuple-gensym-list (tuple-gensyms type)))
              `(defsetf  ,(as-accessor name) (object index)  ,tuple-gensym-list
                 `(setf (,',(tuple-symbol type :def-tuple-aref)  
                            (,',(as-private-accessor class-name name) ,object) ,index)
                        (,',(tuple-symbol type :def-tuple) 
                            ,,@tuple-gensym-list)))))
          ;; scalar form
          (progn
            (let ((tuple-gensym-list (tuple-gensyms type)))
              `(defsetf  ,(as-accessor name) (object)  ,tuple-gensym-list
                 `(setf (,',(tuple-symbol type :def-tuple-getter)  
                            (,',(as-private-accessor class-name name) ,object)) 
                        (,',(tuple-symbol type :def-tuple) 
                            ,,@tuple-gensym-list)))))))))


(defmacro def-tuple-class (class-name slot-specs)
  "Define a class that contains optimzed tuple type slots as well as normal CLOS slots."
    `(progn 
       (defclass ,class-name ()
         ,(destructuring-bind 
           (&key tuples slots)  
           slot-specs
           (append
            (mapcar #'(lambda (slot-spec) 
                        (slot->defclass-slot class-name slot-spec)) tuples)
            (mapcar #'identity `(,@slots)))))
       ,@(destructuring-bind 
         (&key tuples slots)
         slot-specs
         (declare (ignore slots))
         (mapcar #'(lambda (slot-spec)
                     (slot->slot-reader class-name slot-spec))
                 tuples))
       ,@(destructuring-bind 
             (&key tuples slots)
           slot-specs
         (declare (ignore slots))
         (mapcar #'(lambda (slot-spec)
                     (slot->slot-writer class-name slot-spec))
                 tuples))))


;; quick test case

;; (def-tuple-class camera
;;      (:tuples
;;       ((up :type cl-tuples::vector3d)
;;        (forward :type vector3d)
;;        (location :type vertex3d)
;;        (vertices :type vertex3d :array 5))
;;       :slots
;;       ((focal-length :type single-float :accessor focal-length-of)
;;        (id :allocation :class :reader id-of))))

;; (defparameter *test-camera* (make-instance 'camera))
;; (setf (up-of *test-camera*) #{ 0.0 0.0 0.0 })
;; (up-of *test-camera*)
;; (setf (up-of *test-camera*) #{ 1.0 2.0 3.0 })
;; (setf (vertices-of *test-camera* 3) #{ 2.0 3.0 -2.5 1.0 })
;; (vertices-of *test-camera* 3) 
;; (vertices-of *test-camera* 4) 
;; (vertices-of *test-camera* 1) 
