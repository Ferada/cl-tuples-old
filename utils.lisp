(in-package :cl-tuples)

;; float that fits within range of x86 hardware register minus tag (rather sbcl oriented)
(deftype fast-float () 
  #+sbcl `(single-float   (#.(- (expt 2f0 64))) (#.(expt 2f0 64)))
  #-sbcl single-float)

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
