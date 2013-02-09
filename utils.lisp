(in-package :cl-tuples)

;; float that fits within range of x86 hardware register minus tag (rather sbcl oriented)
(deftype fast-float () 
  #+sbcl `(single-float   (#.(- (expt 2f0 64))) (#.(expt 2f0 64)))
  #-sbcl single-float)

(defconstant fast-pi
  #.(coerce pi 'fast-float))

(defun make-adorned-symbol (name &key prefix suffix asterisk package)
  (check-type name (or string symbol))
  (check-type prefix (or symbol string null))
  (check-type suffix (or symbol string null))
  (intern (concatenate 'string
                       (when prefix
                         (string prefix))
                       (when prefix  "-")
                       (string name)
                       (when suffix
                         "-")
                       (when suffix
                         (string suffix))
                       (when asterisk
                         (string "*")))
          (if package package *package*)))

(defmacro multiply-arguments (operator factor arguments)
  `(,operator ,@(mapcar (lambda (argument) `(* ,factor ,argument)) arguments)))

(defun matrix-symbol (i j &optional (prefix '#:e))
  (find-symbol (format NIL "~A~D~D" prefix i j)))

(defun matrix-minor (x y length &optional (prefix '#:e))
  (let ((symbol-prefix (format NIL "~A~D~:*~D" '#:matrix (1- length))))
    `(,(find-symbol (concatenate 'string symbol-prefix #.(string '#:-determinant*)))
      (,(find-symbol (concatenate 'string symbol-prefix #.(string '#:-values*)))
       ,@(iterate values
           (for i from 1 to length)
           (iterate
             (for j from 1 to length)
             (unless (or (eql i x) (eql j y))
               (in values (collect (matrix-symbol (1- i) (1- j) prefix))))))))))

(defun matrix-cofactors (length)
  (iterate values
    (for i from 1 to length)
    (iterate
      (for j from 1 to length)
      (for value = (matrix-minor i j length))
      (in values
          (collect (if (oddp (+ i j))
                       `(- ,value)
                       value))))))
