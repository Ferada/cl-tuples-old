(in-package :cl-tuples)

(defmacro def-tuple-class (class-name slot-specs)
  "(def-tuple-class (:tuples (..) :slots (..))) Define a class that contains optimzed tuple type slots as well as normal CLOS slots."
    `(progn
       ;; expand tuple slots
       (defclass ,class-name ()
         ,(destructuring-bind
           (&key tuples slots)
           slot-specs
           (append
            (mapcar #'(lambda (slot-spec)
                        (slot->defclass-slot class-name slot-spec)) tuples)
            (mapcar #'identity `(,@slots)))))
       ;; expand tuple getters
       ,@(destructuring-bind
         (&key tuples slots)
         slot-specs
         (declare (ignore slots))
         (mapcar #'(lambda (slot-spec)
                     (slot->slot-reader class-name slot-spec))
                 tuples))
       ;; expand tuple setters
       ,@(destructuring-bind
             (&key tuples slots)
           slot-specs
         (declare (ignore slots))
         (mapcar #'(lambda (slot-spec)
                     (slot->slot-writer class-name slot-spec))
                 tuples))))


