(in-package :cl-tuples)

;; to do -- investigate cl-syntax-sugar to see if we can come up with
;; some nicer custom syntax

;; make #{ .. } notation become a short hand for (values ...)
(defun |#{-reader| (stream char arg)
  (declare (ignore char arg))
  `(values ,@(read-delimited-list #\} stream t)))

(defvar *original-readtable* NIL)

(defvar *restore-reader-syntax* NIL)

(defmacro disable-tuples-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *restore-reader-syntax* NIL)
    (%disable-tuples-syntax)))

(defmacro locally-disable-tuples-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-tuples-syntax)))

(defun %disable-tuples-syntax ()
  (when *original-readtable*
    (setf *readtable* *original-readtable*
          *original-readtable* NIL))
  (values))

(defmacro enable-tuples-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *restore-reader-syntax* T)
    (%enable-tuples-syntax)))

(defmacro locally-enable-tuples-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-tuples-syntax)))

(defmacro file-enable-tuples-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-tuples-syntax NIL)))

(defun %enable-tuples-syntax (&optional (save-original-p T))
  (when save-original-p
    (setf *original-readtable* (copy-readtable)))
  (when (or (not save-original-p) *original-readtable*)
    (setf *readtable* (copy-readtable))
    (set-dispatch-macro-character #\# #\{ #'|#{-reader|)
    (set-macro-character #\} (get-macro-character #\) nil)))
  (values))

(defmacro restore-tuples-syntax-state ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (if *restore-tuples-syntax*
        (%enable-tuples-syntax)
        (%disable-tuples-syntax))))
