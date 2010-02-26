;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;; TODO: move this stuff to hu.dwim.util?

;;;;;;
;;; Dictionary

(def (namespace e) dictionary (args &body definition-names)
  (bind ((quoted-definition-names (mapcar [list 'quote !1] definition-names)))
    `(make-instance 'dictionary ,@args
                    :name ',-name-
                    :definition-names (list ,@quoted-definition-names))))

(def class* dictionary ()
  ((name :type symbol)
   (definition-names :type list)
   (documentation nil :type string))
  (:documentation "A DICTIONARY is a list of definition names referring to the related definitions."))

;; TODO: move?
(def method localized-instance-name ((dictionary dictionary))
  (lookup-resource (string+ "dictionary-name." (string-downcase (name-of dictionary)))))
