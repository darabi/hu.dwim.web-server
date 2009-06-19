;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Content basic

(def (component e) content/basic (content/abstract component/basic style/abstract)
  ()
  (:documentation "A COMPONENT with style, remote setup and another COMPONENT inside."))

(def (macro e) content/basic ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'content/basic ,@args :content ,(the-only-element content)))

(def render-xhtml content/basic
  (with-render-style/abstract (-self-)
    (call-next-method)))

;;;;;;
;;; Content full

(def (component e) content/full (content/basic component/full)
  ()
  (:documentation "A COMPONENT with style, remote setup and another COMPONENT inside."))

;;;;;;
;;; Content

(def (macro e) content ((&rest args &key &allow-other-keys) &body content)
  `(content/basic ,args ,@content))

;;;;;;
;;; XHTML content basic

(def (component e) xhtml-content/basic (content/mixin)
  ((content :type string))
  (:documentation "A COMPONENT with an XHTML string content inside."))

(def render-xhtml xhtml-content/basic
  (write-sequence (babel:string-to-octets (content-of -self-) :encoding :utf-8) *xml-stream*)
  (values))
