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
    (call-next-method)))

;;;;;;
;;; XHTML content widget

(def (component e) xhtml-content/widget (widget/basic content/mixin)
  ((content :type string))
  (:documentation "A COMPONENT with an XHTML string content inside."))

(def render-xhtml xhtml-content/widget
  (write-sequence (babel:string-to-octets (content-of -self-) :encoding :utf-8) *xml-stream*)
  (values))
