;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; button/component

(def (component e) button/component (standard/widget content/component)
  ())

;;;;;;
;;; push-button/widget

(def (component e) push-button/widget (button/component)
  ())

(def (macro e) push-button/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'push-button/widget ,@args :content ,(the-only-element content)))

(def render-xhtml push-button/widget
  ;; TODO: add javascript to change border on the client side when clicked
  (with-render-style/component (-self-)
    (render-content-for -self-)))

;;;;;;
;;; toggle-button/widget

(def (component e) toggle-button/widget (button/component)
  ((pushed-in :type boolean)))

(def (macro e) toggle-button/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'toggle-button/widget ,@args :content ,(the-only-element content)))

(def render-xhtml toggle-button/widget
  ;; TODO: setup border based on pushed-in
  (with-render-style/component (-self-)
    (render-content-for -self-)))

;;;;;;
;;; drop-down-button/widget

;; TODO: add contents and what? is it really a button or what?
(def (component e) drop-down-button/widget (button/component)
  ())

(def (macro e) drop-down-button/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'drop-down-button/widget ,@args :content ,(the-only-element content)))

(def render-xhtml drop-down-button/widget
  (with-render-style/component (-self-)
    (render-content-for -self-)))
