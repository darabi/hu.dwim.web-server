;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Content widget

(def (component e) content/widget (widget/style content/abstract context-menu/mixin)
  ()
  (:documentation "A COMPONENT with style, remote setup, context menu and another COMPONENT inside."))

(def (macro e) content/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'content/widget ,@args :content ,(the-only-element content)))

(def render-xhtml content/widget
  (with-render-style/abstract (-self-)
    (render-context-menu-for -self-)
    (render-content-for -self-)))

;;;;;;
;;; Inline XHTML string content widget

(def (component e) inline-xhtml-string-content/widget (widget/basic content/abstract style/mixin)
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
;;; Quote XML string content widget

(def (component e) quote-xml-string-content/widget (widget/basic content/abstract style/mixin)
  ((content :type string))
  (:documentation "A COMPONENT that renders its content as a quoted XML string."))

(def (macro e) quote-xml-string-content/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'quote-xml-string-content/widget ,@args :content ,(the-only-element content)))

(def render-xhtml quote-xml-string-content/widget
  (with-render-style/mixin (-self- :element-name "pre")
    (render-content-for -self-)))

;;;;;;
;;; Quote XML form widget

(def (component e) quote-xml-form/widget (widget/basic style/mixin thunk/mixin)
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
