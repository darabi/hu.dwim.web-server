;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Interaction

(def special-variable *interaction*)

(def class* interaction ()
  ((aborted #f :type boolean :accessor aborted?))
  (:documentation "An INTERACTION is a process initiated by the user that may either finish or abort. When an INTERACTION aborts the user will be notified with an ERROR-MESSAGE."))

(def (with-macro e) with-interaction (component)
  "Wraps the forms inside with an INTERACTION related to COMPONENT."
  (bind ((*interaction* (make-instance 'interaction)))
    (unwind-protect (-body-)
      (when (interaction-aborted?)
        (add-component-error-message component #"interaction-aborted")))))

(def (function e) abort-interaction (&optional (interaction *interaction*))
  (setf (aborted? interaction) #t))

(def (function e) interaction-aborted? (&optional (interaction *interaction*))
  (aborted? interaction))
