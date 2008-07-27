;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code adapted from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; Some derived portions (C) 2008 John Connors


;;; From student.lisp:
(defstruct (rule (:type list)) pattern response)
(defstruct (exp (:type list)
                (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun exp-p (x) (consp x))
(defun exp-args (x) (rest x))

(defun binary-exp-p (x)
  (and (exp-p x) (= (length (exp-args x)) 2)))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar #'prefix->infix
              (if (binary-exp-p exp)
                  (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
                  exp))))

;; Define x+ and y+ as a sequence:
(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

;; Define n and m as numbers; s as a non-number:
(pat-match-abbrev 'n '(?is n numberp))
(pat-match-abbrev 'm '(?is m numberp))
(pat-match-abbrev 's '(?is s not-numberp))

;; defint t as a tuple-type
(pat-match-abbrev 't '(?is t tuple-typep))


(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
    '(((x+ = y+) (= x y))
      ((- x+)    (- x))
      ((+ x+)    (+ x))
      ((x+ + y+) (+ x y))
      ((x+ - y+) (- x y))
      ((d y+ / d x) (d y x))        ;*** New rule
      ((Int y+ d x) (int y x))      ;*** New rule
      ((x+ * y+) (* x y))
      ((x+ / y+) (/ x y))
      ((x+ ^ y+) (^ x y)))))

(defun infix->prefix (exp)
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  (cond ((atom exp) exp)
        ((= (length exp) 1) (infix->prefix (first exp)))
        ((rule-based-translator exp *infix->prefix-rules*
           :rule-if #'rule-pattern :rule-then #'rule-response
           :action
           #'(lambda (bindings response)
               (sublis (mapcar
                         #'(lambda (pair)
                             (cons (first pair)
                                   (infix->prefix (rest pair))))
                         bindings)
                       response))))
        ((symbolp (first exp))
         (list (first exp) (infix->prefix (rest exp))))
        (t (error "Illegal exp"))))

;;((vector3d thing) + (vector3d thang))