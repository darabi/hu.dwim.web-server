;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; content/widget

(def (component e) content/widget (standard/widget content/component context-menu/mixin)
  ()
  (:documentation "A COMPONENT with style, remote setup, context menu and another COMPONENT inside."))

(def (macro e) content/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'content/widget ,@args :content ,(the-only-element content)))

(def render-component content/widget
  (render-content-for -self-))

(def render-xhtml content/widget
  (with-render-style/component (-self-)
    (render-context-menu-for -self-)
    (render-content-for -self-)))

;;;;;;
;;; contents/widget

(def (component e) contents/widget (standard/widget contents/component context-menu/mixin)
  ()
  (:documentation "A COMPONENT with style, remote setup, context menu and several COMPONENTs inside."))

(def (macro e) contents/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'contents/widget ,@args :contents (list ,@contents)))

(def render-component contents/widget
  (render-contents-for -self-))

(def render-xhtml contents/widget
  (with-render-style/component (-self-)
    (render-context-menu-for -self-)
    (render-contents-for -self-)))

;;;;;;
;;; inline-xhtml-string-content/widget

(def (component e) inline-xhtml-string-content/widget (standard/widget content/component)
  ((content :type string))
  (:documentation "A COMPONENT that renders its content as inline XHTML."))

(def (macro e) inline-xhtml-string-content/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'inline-xhtml-string-content/widget ,@args :content ,(the-only-element content)))

(def render-component inline-xhtml-string-content/widget
  (error "Cannot render ~A in the current format" -self-))

(def render-xhtml inline-xhtml-string-content/widget
  (bind (((:read-only-slots content) -self-))
    (with-render-style/mixin (-self-)
      (if (eq (stream-element-type *xml-stream*) :default)
          (write-string content *xml-stream*)
          (write-sequence (babel:string-to-octets content :encoding :utf-8) *xml-stream*))
      (values))))

;;;;;;
;;; quote-xml-string-content/widget

(def (component e) quote-xml-string-content/widget (standard/widget content/component)
  ((content :type string))
  (:documentation "A COMPONENT that renders its content as a quoted XML string."))

(def (macro e) quote-xml-string-content/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'quote-xml-string-content/widget ,@args :content ,(the-only-element content)))

(def render-xhtml quote-xml-string-content/widget
  (with-render-style/mixin (-self- :element-name "pre")
    (render-content-for -self-)))

;;;;;;
;;; quote-xml-form/widget

(def (component e) quote-xml-form/widget (standard/widget thunk/mixin)
  ()
  (:documentation "A COMPONENT that renders its content provided as a FORM as a quoted XML string."))

(def (macro e) quote-xml-form/widget ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 'quote-xml-form/widget ,@args :thunk (lambda () ,@forms)))

(def render-xhtml quote-xml-form/widget
  (with-render-style/mixin (-self- :element-name "pre")
    ;; TODO: delete this comment as soon as we can work with application/xhtml+xml content type
    ;; NOTE: if you think this does not work then you are wrong
    ;;       it works as long as the content-type is application/xhtml+xml (which is not right now)
    (write-sequence "<![CDATA[" *xml-stream*)
    (funcall (thunk-of -self-))
    (write-sequence "]]>" *xml-stream*)
    (values)))
