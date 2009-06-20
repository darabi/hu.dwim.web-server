;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Component messages basic

(def (component e) component-messages/basic (component/basic)
  ((messages nil :type components))
  (:documentation "A COMPONENT with a list of messages."))

(def (macro e) component-messages/basic ((&rest args &key &allow-other-keys) &body messages)
  `(make-instance 'component-messages/basic ,@args :messages (list ,@messages)))

(def render-xhtml component-messages/basic
  (render-component-messages -self-))

(def (function e) render-component-messages (collector)
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

(def method add-component-information-message ((collector component-messages/basic) message &rest message-args)
  (add-component-message collector message message-args :category :information))

(def method add-component-warning-message ((collector component-messages/basic) message &rest message-args)
  (add-component-message collector message message-args :category :warning))

(def method add-component-error-message ((collector component-messages/basic) message &rest message-args)
  (add-component-message collector message message-args :category :error))

(def method add-component-message ((collector component-messages/basic) message message-args &rest initargs &key category &allow-other-keys)
  (appendf (messages-of collector)
           (list (if (typep message 'component-message/basic)
                     (progn
                       (when category
                         (setf (category-of message) category))
                       message)
                     (apply #'make-instance 'component-message/basic
                            :content (if (stringp message)
                                         (apply #'format nil message message-args)
                                         message)
                            initargs)))))

(def (function e) has-component-message-p (collector category)
  (find category (messages-of collector) :key #'category-of))

;;;;;;
;;; Component messages

(def macro component-messages ((&rest args &key &allow-other-keys) &body content)
  `(component-messages/basic ,args ,@content))

;;;;;;
;;; Component message basic

(def (component e) component-message/basic (component/basic closable/abstract content/abstract style/abstract)
  ((category :information :type (member :information :warning :error))
   (permanent #f :type boolean)))

(def macro component-message/basic ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'component-message/basic ,@args :content ,(the-only-element content)))

(def refresh-component component-message/basic
  (bind (((:slots category style-class) -self-))
    (unless style-class
      (setf style-class (concatenate-string (string-downcase category) "-message")))))

(def render-xhtml component-message/basic
  (with-render-style/abstract (-self-)
    (call-next-method)))

(def layered-method make-close-component-command ((component component-message/basic) class prototype value)
  (when (permanent? component)
    (call-next-method)))

(def layered-method close-component ((component component-message/basic) class prototype value)
  (deletef (messages-of (parent-component-of component)) component))

;;;;;;
;;; Component message

(def macro component-message ((&rest args &key &allow-other-keys) &body content)
  `(component-message/basic ,args ,@content))
