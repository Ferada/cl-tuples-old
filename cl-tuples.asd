;;;; Silly emacs, this is -*- Lisp -*-

(in-package :asdf)

(defsystem :cl-tuples
  :name "cl-tuples"
  :author "John Connors"
  :version "1.0"
  :licence "MIT"
  :description "Experimental Tuple Types"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "symbols")
               (:file "tuple-expander")
               (:file "tuples") 
               (:file "clos-wrapper")
               (:file "vector")
               (:file "quaternion")
               (:file "matrix")
               (:file "colour")
               (:file "triangle")
               (:file "rect")))

(defmethod perform ((o test-op) (c (eql (find-system :cl-tuples))))
  (operate 'asdf:load-op :cl-tuples-tests)
  (operate 'asdf:test-op :cl-tuples-tests))
                  
