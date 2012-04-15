

(in-package :cl-tuples)

;; float that fits within range of x86 hardware flops
(deftype fast-float () `(single-float   (#.(- (expt 2f0 64))) (#.(expt 2f0 64))))

(defmacro with-gensyms ((&rest names) &body body)
  "Classic macro for creating named unique symbols."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  "Evaluate arguments once only in macro form body"
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))

;; define helper functions we will use

(defun gensym-list (n)
  "Give us a list of gensyms n elements long"
  (loop
     for index from 0 below n
     collect (gensym)))


(defun symbol-to-string (x)
  "If the argument is a symbol or string, return it as a string."
  (check-type x (or symbol string))
  (cond
    ((symbolp x)
     (symbol-name x))
    ((stringp x)
     x)))


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

(defun make-suffixed-symbol (name suffix)
  (make-adorned-symbol name :suffix suffix))

(defun make-prefixed-symbol (name prefix)
  (make-adorned-symbol name :prefix prefix))

(defun is-asterisk-symbol (s)
  (let 
	  ((ss (symbol-to-string s)))
	(eql (aref ss (1- (length ss))) #\*)))

(defun make-element-names (elements type-name)
  "Given a list of element names form a set of symbols of the form
     <type-name>-<element-name> as used in struct elements."
  (check-type elements symbol)
  (check-type type-name symbol)
  (mapcar #'(lambda (x)
              (find-symbol
               (concatenate 'string
                            (symbol-name type-name) "-struct-"
                            (symbol-name x))))
          elements))

