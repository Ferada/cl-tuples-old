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
               (:file "symbols")
               (:file "tuples") 
               (:file "vector")
               (:file "quaternion")
               (:file "matrix")
               (:file "colour")))

(defun vaporise-wip ()
          (let ((package-to-vaporise *package*))
            (in-package :cl-user)
            (delete-package package-to-vaporise)))
                  
