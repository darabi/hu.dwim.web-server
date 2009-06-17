;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Interaction

(def special-variable *interaction*)

(def class* interaction ()
  ((aborted #f :type boolean))
  (:documentation "An interaction is a process initiated by the user that may either finish or abort."))

(def (with-macro e) with-interaction (component)
  (bind ((*interaction* (make-instance 'interaction)))
    (unwind-protect (-body-)
      (when (interaction-aborted?)
        (add-user-error component #"interaction-aborted")))))

(def (function e) abort-interaction (&optional (interaction *interaction*))
  (setf (aborted? interaction) #t))

(def (function e) interaction-aborted? (&optional (interaction *interaction*))
  (aborted? interaction))

;;;;;;
;;; Localization

(def resources hu
  (interaction-aborted "Kérem javítsa a megjelölt hibákat és próbálja újra a műveletet"))

(def resources en
  (interaction-aborted "Please correct the errors and try again"))
