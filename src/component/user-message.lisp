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
  (render-user-messages -self-))

(def component user-message-collector-wrapper-component (user-message-collector-component)
  ((body :type component)))

(def (macro e) user-message-collector-wrapper-component (&body components)
  `(make-instance 'user-message-collector-wrapper-component
                  :body ,(if (length= 1 components)
                             (first components)
                             `(make-instance 'vertical-list-component :body (remove nil (list ,@components))))))

(def render user-message-collector-wrapper-component ()
  (render-user-messages -self-)
  (render (body-of -self-)))

(def (function e) render-user-messages (user-message-collector)
  (bind ((messages (messages-of user-message-collector)))
    (remove-user-messages-if user-message-collector (complement #'permanent-p))
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

(def (function e) remove-user-messages-if (collector predicate)
  (setf (messages-of collector) (delete-if predicate (messages-of collector))))

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
                          :message (if (stringp message)
                                       (apply #'format nil message message-args)
                                       message)
                          initargs)))))

(def (function e) has-user-message-p (collector category)
  (find category (messages-of collector) :key #'category-of))

;;;;;;
;;; User message

(def component user-message-component (content-component style-component-mixin)
  ((category :information :type (member :information :warning :error))
   (message nil :type string)
   (permanent #f :type boolean)))

(def render user-message-component ()
  (bind (((:read-only-slots category message permanent content css-class style) -self-)
         (id (generate-response-unique-string)))
    <div (:id ,id :class ,(concatenate-string (string-downcase category) " " css-class) :style ,style)
         ,(when permanent
            (render (command (icon close :label nil)
                             (make-action
                               (removef (messages-of (parent-component-of -self-)) -self-)))))
         ,(render message)
         ,(when content
            (call-next-method))>
    `js(wui.setup-widget "user-message" ,id (create :css-class ,(concatenate-string (string-downcase category) "-message")))))
