;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; User message collector mixin

(def component user-message-collector-component-mixin ()
  ((messages nil :type components)))

;;;;;;
;;; User message collector

(def component user-message-collector-component (user-message-collector-component-mixin)
  ())

(def render user-message-collector-component ()
  (with-slots (messages) -self-
    (prog1 (render-vertical-list messages)
      (setf messages (delete-if (complement #'permanent-p) messages)))))

(def (generic e) add-user-message (collector message &rest args)
  (:method ((collector user-message-collector-component-mixin) message &rest args)
    (setf (messages-of collector)
          (append (messages-of collector)
                  (list (apply #'make-instance 'user-message-component :message message args))))))

;;;;;;
;;; User message

(def component user-message-component ()
  ((category :information :type (member :information :warning :error))
   (message nil :type string)
   (permanent #f :type boolean)))

(def render user-message-component ()
  (with-slots (category message args) -self-
    <span (:class ,(string-downcase category))
          ,message>))
