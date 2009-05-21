;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; User message mixin

(def component user-messages-mixin ()
  ((messages nil :type components))
  (:documentation "A component with a list of user messages."))

;;;;;;
;;; User message component

(def component user-messages-component (user-messages-mixin)
  ())

(def render-xhtml user-messages-component
  (render-user-messages -self-))

(def (function e) render-user-messages (collector)
  (bind ((messages (messages-of collector)))
    (flet ((render-message-category (category)
             (awhen (filter category messages :key #'category-of)
               <div (:class ,(concatenate-string "user-messages " (string-downcase category) "s"))
                    ,(render-vertical-list it)>)))
      (multiple-value-prog1
          (when messages
            <div ,(render-message-category :information)
                 ,(render-message-category :warning)
                 ,(render-message-category :error)>)
        (remove-user-messages-if collector (complement #'permanent-p))))))

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
           (find-ancestor-component-with-type component 'user-messages-mixin)
           message message-args initargs))

  (:method ((collector user-messages-mixin) message message-args &rest initargs &key content category &allow-other-keys)
    (assert (typep content '(or null component)))
    (appendf (messages-of collector)
             (list (if (typep message 'user-message-component)
                       (progn
                         (when category
                           (setf (category-of message) category))
                         message)
                       (apply #'make-instance 'user-message-component
                              :message (if (stringp message)
                                           (apply #'format nil message message-args)
                                           message)
                              initargs))))))

(def (function e) has-user-message-p (collector category)
  (find category (messages-of collector) :key #'category-of))

;;;;;;
;;; User message component

(def component user-message-component (content-mixin style-mixin)
  ((category :information :type (member :information :warning :error))
   (message nil :type string)
   (permanent #f :type boolean)))

(def render-xhtml user-message-component
  (bind (((:read-only-slots category message permanent content css-class style) -self-)
         (id (generate-response-unique-string)))
    <div (:id ,id :class ,(concatenate-string (string-downcase category) " " css-class) :style ,style)
         ,(when permanent
            ;; TODO use more specific icon/tooltip for this action
            (render (command (icon close :label nil)
                             (make-action
                               (deletef (messages-of (parent-component-of -self-)) -self-)))))
         ,(render message)
         ,(when content
            (call-next-method))>
    `js(wui.setup-component "user-message-component" ,id (create :css-class ,(concatenate-string (string-downcase category) "-message")))))
