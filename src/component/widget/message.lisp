;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; User message mixin

(def (component ea) user-messages/mixin ()
  ((messages nil :type components))
  (:documentation "A component with a list of user messages."))

;;;;;;
;;; User message component

;; TODO: do we use/need this?
(def (component ea) user-messages/basic (user-messages/mixin)
  ())

(def render-xhtml user-messages/basic
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
        (remove-user-messages-if collector (complement #'permanent?))))))

(def (function e) remove-user-messages-if (collector predicate)
  (setf (messages-of collector) (delete-if predicate (messages-of collector))))

(def method add-user-information ((collector user-messages/basic) message &rest message-args)
  (add-user-message collector message message-args :category :information))

(def method add-user-warning ((collector user-messages/basic) message &rest message-args)
  (add-user-message collector message message-args :category :warning))

(def method add-user-error ((collector user-messages/basic) message &rest message-args)
  (add-user-message collector message message-args :category :error))

(def method add-user-message ((component user-messages/basic) message message-args &rest initargs)
  (assert (typep content '(or null component)))
  (appendf (messages-of collector)
           (list (if (typep message 'user-message/basic)
                     (progn
                       (when category
                         (setf (category-of message) category))
                       message)
                     (apply #'make-instance 'user-message/basic
                            :message (if (stringp message)
                                         (apply #'format nil message message-args)
                                         message)
                            initargs)))))

(def (function e) has-user-message-p (collector category)
  (find category (messages-of collector) :key #'category-of))

;;;;;;
;;; User message component

(def (component ea) user-message/basic (closable/mixin content/mixin style/mixin)
  ((category :information :type (member :information :warning :error))
   (message nil :type string)
   (permanent #f :type boolean)))

(def layered-method make-close-component-command ((component user-message/basic) class prototype value)
  (when (permanent? component)
    (call-next-method)))

(def render-xhtml user-message/basic
  (bind (((:read-only-slots category message content css-class style) -self-)
         (id (generate-response-unique-string)))
    <div (:id ,id :class ,(concatenate-string (string-downcase category) " " css-class) :style ,style)
         ,(render-component message)
         ,(when content
            (call-next-method))>
    `js(wui.setup-component ,id "user-message/basic" (create :css-class ,(concatenate-string (string-downcase category) "-message")))))

(def layered-method execute-close-component ((component user-message/basic) class prototype value)
  (deletef (messages-of (parent-component-of component)) component))
