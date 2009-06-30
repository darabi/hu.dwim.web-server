;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Component messages

(def (component e) component-messages ()
  ())

(def macro component-messages ((&rest args &key &allow-other-keys) &body content)
  `(component-messages/widget ,args ,@content))

;;;;;;
;;; Component messages widget

(def (component e) component-messages/widget (component-messages widget/basic)
  ((messages nil :type components))
  (:documentation "A COMPONENT with a list of COMPONENT-MESSAGEs."))

(def (macro e) component-messages/widget ((&rest args &key &allow-other-keys) &body messages)
  `(make-instance 'component-messages/widget ,@args :messages (list ,@messages)))

(def render-xhtml component-messages/widget
  (render-component-messages-for -self-))

(def (function e) render-component-messages-for (collector)
  (bind ((messages (messages-of collector)))
    (flet ((render-message-category (category)
             (awhen (filter category messages :key #'category-of)
               <div (:class ,(concatenate-string (string-downcase category) "-messages"))
                    ,(foreach #'render-component it)>)))
      (multiple-value-prog1
          (when messages
            <div (:class "component-messages")
                 ,(render-message-category :information)
                 ,(render-message-category :warning)
                 ,(render-message-category :error)>)
        (remove-component-messages-if collector (complement #'permanent?))))))

(def (function e) remove-component-messages-if (collector predicate)
  (setf (messages-of collector) (delete-if predicate (messages-of collector))))

(def method add-component-information-message ((collector component-messages/widget) message &rest message-args)
  (add-component-message collector message message-args :category :information))

(def method add-component-warning-message ((collector component-messages/widget) message &rest message-args)
  (add-component-message collector message message-args :category :warning))

(def method add-component-error-message ((collector component-messages/widget) message &rest message-args)
  (add-component-message collector message message-args :category :error))

(def method add-component-message ((collector component-messages/widget) message message-args &rest initargs &key category &allow-other-keys)
  (appendf (messages-of collector)
           (list (if (typep message 'component-message/widget)
                     (progn
                       (when category
                         (setf (category-of message) category))
                       message)
                     (apply #'make-instance 'component-message/widget
                            :content (if (stringp message)
                                         (apply #'format nil message message-args)
                                         message)
                            initargs)))))

(def (function e) has-component-message-p (collector category)
  (find category (messages-of collector) :key #'category-of))

;;;;;;
;;; Component message

(def (component e) component-message ()
  ()
  (:documentation "Base COMPONENT for all COMPONENT-MESSAGEs."))

(def macro component-message ((&rest args &key &allow-other-keys) &body content)
  `(component-message/widget ,args ,@content))

;;;;;;
;;; Component message widget

(def (component e) component-message/widget (component-message widget/basic closable/abstract content/abstract style/abstract)
  ((category :information :type (member :information :warning :error))
   (permanent #f :type boolean))
  (:documentation "An optionally permanent COMPONENT-MESSAGE with a CATEGORY. Permanent messages must be removed by explicit user interaction."))

(def macro component-message/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'component-message/widget ,@args :content ,(the-only-element content)))

(def refresh-component component-message/widget
  (bind (((:slots category style-class) -self-))
    (unless style-class
      (setf style-class (concatenate-string (string-downcase category) "-message")))))

(def render-xhtml component-message/widget
  (with-render-style/abstract (-self-)
    (call-next-method)))

(def layered-method make-close-component-command ((component component-message/widget) class prototype value)
  (when (permanent? component)
    (call-next-method)))

(def layered-method close-component ((component component-message/widget) class prototype value)
  (deletef (messages-of (parent-component-of component)) component))
