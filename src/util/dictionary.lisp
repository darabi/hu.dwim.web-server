;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

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

(def (definer e :available-flags "e") dictionary (name &rest names)
  (bind ((quoted-names (mapcar [list 'quote !1] names)))
    `(setf (find-dictionary ',name) (make-instance 'dictionary :names (list ,@quoted-names)))))

;; TODO: move to cl-def or what?
(def (function e) collect-definitions (name)
  ;; TODO: this must be extensible with respect to new definition kinds
  #+nil
  (swank::find-definitions name)
  (not-yet-implemented))
