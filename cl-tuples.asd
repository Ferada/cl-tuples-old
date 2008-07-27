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
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "symbols")
               (:file "tuple-expander")
               (:file "tuples") 
               (:file "clos-wrapper")
               (:file "clos-wrapper-macro")
               (:file "vector")
               (:file "quaternion")
               (:file "matrix")
               (:file "colour")
               (:file "triangle")
               (:file "rect")))

(defsystem :cl-tuples-infix
  :name "paip-infix"
  :licence "http://norvig.com/license.html"
  :author "Peter Norvig and John Connors."
  :description "Pattern matching and infix<->prefix conversion code from Paradigms of Artificial Intelligence Programming applied to cl-tuples"
  :serial t
  :depends-on (:cl-tuples)
  :components ((:file "auxfns")
               (:file "patmatch")
               (:file "prefix-infix")
               (:file "tuple-infix")))

(defmethod perform ((o test-op) (c (eql (find-system :cl-tuples))))
  (operate 'asdf:load-op :cl-tuples-tests)
  (operate 'asdf:test-op :cl-tuples-tests))
                  
