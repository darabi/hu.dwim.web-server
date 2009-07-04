;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Content widget

(def (component e) content/widget (widget/basic content/abstract style/abstract)
  ()
  (:documentation "A COMPONENT with style, remote setup and another COMPONENT inside."))

(def (macro e) content/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'content/widget ,@args :content ,(the-only-element content)))

(def render-xhtml content/widget
  (with-render-style/abstract (-self-)
    (render-content-for -self-)))

;;;;;;
;;; Inline XHTML content widget

(def (component e) inline-xhtml-content/widget (widget/basic content/mixin)
  ((content :type string))
  (:documentation "A COMPONENT with an inline rendered XHTML string content inside."))

(def (macro e) inline-xhtml-content/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'inline-xhtml-content/widget ,@args :content ,(the-only-element content)))

(def render-xhtml inline-xhtml-content/widget
  (write-sequence (babel:string-to-octets (content-of -self-) :encoding :utf-8) *xml-stream*)
  (values))

;;;;;;
;;; Quoted XHTML content widget

(def (component e) quoted-xhtml-content/widget (widget/basic content/mixin)
  ((content :type string))
  (:documentation "A COMPONENT with a quoted XHTML string content inside."))

(def (macro e) quoted-xhtml-content/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'quoted-xhtml-content/widget ,@args :content ,(the-only-element content)))

(def render-xhtml quoted-xhtml-content/widget
  <pre (:class "xhtml")
    ,(render-content-for -self-)>)
