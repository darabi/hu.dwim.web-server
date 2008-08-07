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

(def (function e) render-user-messages (user-message-collector)
  (bind ((messages (messages-of user-message-collector)))
    (setf (messages-of user-message-collector) (delete-if (complement #'permanent-p) messages))
    (flet ((render-message-category (category)
             (aif (filter category messages :key #'category-of)
                  <div (:class ,(concatenate-string "user-messages " (string-downcase category) "s"))
                       ,(render-vertical-list it)>
                  +void+)))
      (if messages
          <div ,(render-message-category :information)
               ,(render-message-category :warning)
               ,(render-message-category :error)>
          +void+))))

(def render user-message-collector-component ()
  (render-user-messages -self-))

(def (function e) add-user-information (collector message &rest message-args)
  (apply #'add-user-message collector message :information message-args))

(def (function e) add-user-warning (collector message &rest message-args)
  (apply #'add-user-message collector message :warning message-args))

(def (function e) add-user-error (collector message &rest message-args)
  (apply #'add-user-message collector message :error message-args))

(def (generic e) add-user-message (collector message category &rest message-args)
  (:method ((component component) message category &rest message-args)
    (apply #'add-user-message
           (find-ancestor-component-with-type component 'user-message-collector-component-mixin)
           category message message-args))

  (:method ((collector user-message-collector-component-mixin) message category &rest message-args)
    (appendf (messages-of collector)
             (list (make-instance 'user-message-component
                                  :message (apply #'format nil message message-args)
                                  :category category)))))

;;;;;;
;;; User message

(def component user-message-component ()
  ((category :information :type (member :information :warning :error))
   (message nil :type string)
   (permanent #f :type boolean)))

(def render user-message-component ()
  (with-slots (category message) -self-
    <div (:class ,(string-downcase category))
         ,message>))
