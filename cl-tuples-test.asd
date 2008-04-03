

(defsystem cl-tuples-tests
  :description "Tests for cl-tuples"
  :depends-on (cl-tuples)
  :components 
  ((:file "tuples-test")))

(defmethod perform ((o test-op) (c (eql (find-system :cl-tuples-tests))))
             (error "test-op-failed")))
           (unless (run-cl-tuples-tests)
