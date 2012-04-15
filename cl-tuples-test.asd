

(defsystem cl-tuples-test
  :description "Tests for cl-tuples"
  :depends-on (:cl-tuples :alexandria)
  :components ((:file "tuples-test")))

(defmethod asdf:perform ((op asdf:test-op) (c (eql (find-system ':cl-tuples))))
  (asdf:operate 'asdf:load-op ':cl-tuples-test)
  (asdf:operate 'asdf:test-op ':cl-tuples-test))


(defmethod asdf:perform ((op asdf:test-op) (c (eql (find-system ':cl-tuples-tests))))
  (funcall (intern "TEST-CL-TUPLES" (find-package "CL-TUPLES-TEST"))))
