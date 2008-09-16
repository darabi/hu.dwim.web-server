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
  (add-user-message collector message message-args :category :information))

(def (function e) add-user-warning (collector message &rest message-args)
  (add-user-message collector message message-args :category :warning))

(def (function e) add-user-error (collector message &rest message-args)
  (add-user-message collector message message-args :category :error))

(def (generic e) add-user-message (collector message message-args &rest initargs &key &allow-other-keys)
  (:method ((component component) message message-args &rest initargs)
    (apply #'add-user-message
           (find-ancestor-component-with-type component 'user-message-collector-component-mixin)
           message message-args initargs))

  (:method ((collector user-message-collector-component-mixin) message message-args &rest initargs &key content &allow-other-keys)
    (assert (typep content '(or null component)))
    (appendf (messages-of collector)
             (list (apply #'make-instance 'user-message-component
                          :message (apply #'format nil message message-args)
                          initargs)))))

;;;;;;
;;; User message

(def component user-message-component (content-component)
  ((category :information :type (member :information :warning :error))
   (message nil :type string)
   (permanent #f :type boolean)))

(def render user-message-component ()
  (bind (((:read-only-slots category message permanent content) -self-))
    <div (:class ,(string-downcase category))
         ,(when permanent
            (render (command (icon close :label nil)
                             (make-action
                               (removef (messages-of (parent-component-of -self-)) -self-)))))
         ,message
         ,(when content
            (call-next-method))>))
