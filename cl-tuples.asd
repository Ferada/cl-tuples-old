;;;; Silly emacs, this is -*- Lisp -*-

(defpackage :cl-tuples-system
  (:use :cl :asdf))

(in-package :cl-tuples-system)

(defsystem :cl-tuples
  :name "cl-tuples"
  :author "John Connors"
  :version "1.0"
  :licence "MIT"
  :description "Experimental Tuple Types Facade"
  :depends-on (:iterate :lexical-rename)
  :serial t
  :components ((:file "package")
			   (:file "utils")
			   (:file "symbols")
			   (:file "tuple-expander")
			   (:file "tuples")
			   (:file "vector")
			   (:file "quaternion")
			   (:file "matrix")
			   (:file "colour")
			   (:file "triangle")
			   (:file "rect")
			   (:file "aabb")))


(defsystem :cl-tuples-tests
  :serial t
  :author "John Connors"
  :version "1.0"
  :licence "MIT"
  :depends-on (:cl-tuples)
  :components ((:file "tuples-test")))

(defmethod perform ((o test-op) (c (eql (find-system :cl-tuples))))
  (operate 'asdf:load-op :cl-tuples-tests)
  (operate 'asdf:test-op :cl-tuples-tests))

