(in-package :cl-tuples)

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


