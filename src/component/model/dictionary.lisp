;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Dictionary definer

(def special-variable *dictionaries* (make-hash-table))

(def class* dictionary ()
  ((symbols :type list)))

(def (function e) find-dictionary (name)
  (gethash name *dictionaries*))

(def (function e) (setf find-dictionary) (new-value name)
  (setf (gethash name *dictionaries*) new-value))

(def (definer e :available-flags "e") dictionary (name &rest symbols)
  (bind ((quoted-symbols (mapcar [list 'quote !1] symbols)))
    `(setf (find-dictionary ',name) (make-instance 'dictionary :symbols (list ,@quoted-symbols)))))

;;;;;;
;;; Dictionary basic

(def (component e) dictionary/basic ()
  ((dictionary :type dictionary))
  (:documentation "A DICTIONARY is a list of symbols with the related definitions."))

(def refresh-component dictionary/basic
  (not-yet-implemented))

(def render-component dictionary/basic
  (not-yet-implemented))
