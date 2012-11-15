(in-package :cl-tuples)

;; float that fits within range of x86 hardware flops
(deftype fast-float ()
  `(single-float (#.(- (expt 2f0 64))) (#.(expt 2f0 64))))

;; define helper functions we will use

(defun make-adorned-symbol (name &key prefix suffix asterisk package)
  (check-type name symbol)
  (check-type prefix (or symbol string null))
  (check-type suffix (or symbol string null))
#+cl-tuples-debug (break)
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
