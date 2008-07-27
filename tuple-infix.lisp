
;; tuple-infix.lisp
;; use Peter Norvigs infix->prefix code in conjunction with tuple-forms

(in-package cl-tuples)

(defmethod tuple-symbol ((type-name symbol) (expansion (eql :def-tuple-infix)))
  (make-adorned-symbol type-name :suffix "INFIX*" :package :cl-tuples))

(defmethod tuple-expansion-fn ((type-name symbol) (expansion (eql :def-tuple-infix)))
  `(defmacro ,(tuple-symbol type-name :def-tuple-infix) (&rest expressions)
     `(,',(tuple-symbol type-name :def-tuple)
          ,@(loop
               for expression in expressions 
               collecting `(the ,',(tuple-element-type type-name) ,(infix->prefix expression))))))

(defmacro def-tuple-infix (type-name)
  "Create macro to evaluate it's parameters to an instance of tuple type with infix expressoins for elements"
  (tuple-expansion-fn :def-tuple-infix))

  