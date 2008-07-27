;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code adapted from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; Some derived portions (C) 2008 John Connors

(in-package :cl-tuples)

;;; From student.lisp:
(defstruct (rule (:type list)) pattern response)
(defstruct (expression (:type list)
                (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun expression-p (x) (consp x))
(defun expression-args (x) (rest x))

(defun binary-expression-p (x)
  (and (expression-p x) (= (length (expression-args x)) 2)))

(defun prefix->infix (expression)
  "Translate prefix to infix expressions."
  (if (atom expression) expression
      (mapcar #'prefix->infix
              (if (binary-expression-p expression)
                  (list (expression-lhs expression) (expression-op expression) (expression-rhs expression))
                  expression))))

;; Define x+ and y+ as a sequence:
(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

;; Define n and m as numbers; s as a non-number:
(pat-match-abbrev 'n '(?is n numberp))
(pat-match-abbrev 'm '(?is m numberp))
(pat-match-abbrev 's '(?is s not-numberp))

;; defint t as a tuple-type
(pat-match-abbrev 'v '(?is ?v tuple-typep))


(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
    '(((- x+)    (- x))
      ((+ x+)    (+ x))
      ((x+ + y+) (+ x y))
      ((x+ - y+) (- x y))
      ((d y+ / d x) (d y x))        ;*** New rule
      ((Int y+ d x) (int y x))      ;*** New rule
      ((x+ * y+) (* x y))
      ((x+ / y+) (/ x y))
      ((x+ ^ y+) (^ x y)))) 
  "Rules to translate from infix to prefix")


(defun infix->prefix (expression)
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  (cond ((atom expression) expression)
        ((= (length expression) 1) (infix->prefix (first expression)))
        ((rule-based-translator expression *infix->prefix-rules*
           :rule-if #'rule-pattern :rule-then #'rule-response
           :action
           #'(lambda (bindings response)
               (identity (list :bindings bindings :response response)))))))

(defun infix->prefix (expression)
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  (cond ((atom expression) expression)
        ((= (length expression) 1) (infix->prefix (first expression)))
        ((rule-based-translator expression *infix->prefix-rules*
           :rule-if #'rule-pattern :rule-then #'rule-response
           :action
           #'(lambda (bindings response)
               (sublis (mapcar
                         #'(lambda (pair)
                             (cons (first pair)
                                   (infix->prefix (rest pair))))
                         bindings)
                       response))))
        ((symbolp (first expression))
         (list (first expression) (infix->prefix (rest expression))))
        (t (error "Illegal expression"))))

