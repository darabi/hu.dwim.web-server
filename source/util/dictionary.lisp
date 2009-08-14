;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;; TODO: move this stuff to hu.dwim.util?

;;;;;;
;;; Dictionaries

(def special-variable *dictionaries* (make-hash-table))

(def (function e) find-dictionary (name)
  (gethash name *dictionaries*))

(def (function e) (setf find-dictionary) (new-value name)
  (setf (gethash name *dictionaries*) new-value))

;;;;;;
;;; Dictionary

(def class* dictionary ()
  ((names :type list :documentation "Names are symbols."))
  (:documentation "A DICTIONARY is a list of names referring to the related definitions."))

;;;;;;
;;; Definer

(def (definer e :available-flags "e") dictionary (name args &body names)
  (bind ((quoted-names (mapcar [list 'quote !1] names)))
    `(progn
       (setf (find-dictionary ',name) (make-instance 'dictionary ,@args :names (list ,@quoted-names)))
       ,@(when (getf -options- :export)
                   `((eval-when (:compile-toplevel :load-toplevel :execute)
                       (export ',name)))))))

;; TODO: move to hu.dwim.def or what?
(def (function e) collect-definitions (name)
  ;; TODO: this must be extensible with respect to new definition kinds
  #+nil
  (swank::find-definitions name)
  (not-yet-implemented))
