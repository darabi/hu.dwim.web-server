;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Content mixin

(def (component ea) content/mixin ()
  ((content
    (empty)
    :type component*
    :documentation "The content of this component, empty by default."))
  (:documentation "A component that has another component inside."))

(def render content/mixin
  (render-content -self-))

(def (layered-function e) render-content (component)
  (:method ((self content/mixin))
    (render-component (content-of self))))

;;;;;;
;;; Content abstract

(def (component ea) content/abstract (refreshable/mixin content/mixin)
  ())

(def refresh content/abstract
  (mark-to-be-refreshed (content-of -self-)))

(def method component-value-of ((self content/abstract))
  (component-value-of (content-of self)))

(def method (setf component-value-of) (new-value (self content/abstract))
  (setf (component-value-of (content-of self)) new-value))

;;;;;;
;;; XHTML content mixin

(def (component ea) xhtml-content/mixin (content/mixin)
  ((content :type string))
  (:documentation "A component with an XHTML string content inside."))

(def render-xhtml xhtml-content/mixin
  (write-sequence (babel:string-to-octets (content-of -self-) :encoding :utf-8) *xml-stream*)
  (values))

;;;;;;
;;; Content basic

(def (component ea) content/basic (content/abstract component/basic style/abstract)
  ()
  (:documentation "A component with style, remote setup and another component inside."))

(def (macro e) content/basic ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'content/basic ,@args :content ,(the-only-element content)))

(def render-xhtml content/basic
  (with-render-style/abstract (-self-)
    (call-next-method)))

;;;;;;
;;; Content basic

(def (component ea) content/full (content/basic component/full)
  ()
  (:documentation "A component with style, remote setup and another component inside."))

;;;;;;
;;; Content

(def (macro e) content ((&rest args &key &allow-other-keys) &body content)
  `(content/basic ,args ,@content))

;;;;;;
;;; Contents mixin

(def (component ea) contents/mixin ()
  ((contents
    (empty)
    :type components))
  (:documentation "A component that has a set of components inside."))

(def render contents/mixin
  (render-contents -self-))

(def (function e) render-contents (component)
  (foreach #'render-component (contents-of component)))

;;;;;;
;;; Contents abstract

(def (component ea) contents/abstract (refreshable/mixin contents/mixin)
  ())

(def refresh contents/abstract
  (map nil 'mark-to-be-refreshed (contents-of -self-)))

(def method component-value-of ((self contents/abstract))
  (mapcar 'component-value-of (contents-of self)))

(def method (setf component-value-of) (new-value (self contents/abstract))
  (mapcar [setf (component-value-of !1) !2] (contents-of self) new-value))
