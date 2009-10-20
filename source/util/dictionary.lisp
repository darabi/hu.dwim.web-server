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
  ((name :type symbol)
   (definition-names :type list)
   (documentation nil :type string))
  (:documentation "A DICTIONARY is a list of definition names referring to the related definitions."))

;;;;;;
;;; Definer

(def (definer e :available-flags "e") dictionary (name args &body definition-names)
  (bind ((quoted-definition-names (mapcar [list 'quote !1] definition-names)))
    `(progn
       (setf (find-dictionary ',name) (make-instance 'dictionary ,@args
                                                     :name ',name
                                                     :definition-names (list ,@quoted-definition-names)))
       ,@(when (getf -options- :export)
                   `((eval-when (:compile-toplevel :load-toplevel :execute)
                       (export ',name)))))))

;; TODO: move?
(def method localized-instance-name ((dictionary dictionary))
  (lookup-resource (string+ "dictionary-name." (string-downcase (name-of dictionary)))))
