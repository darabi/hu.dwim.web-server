;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Button widget

(def (component e) button/widget (widget/basic style/abstract content/abstract)
  ())

;;;;;;
;;; Push button widget

(def (component e) push-button/widget (button/widget)
  ())

(def (macro e) push-button/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'push-button/widget ,@args :content ,(the-only-element content)))

(def render-xhtml push-button/widget
  ;; TODO: add javascript to change border on the client side when clicked
  (with-render-style/abstract (-self-)
    (render-content-for -self-)))

;;;;;;
;;; Toggle button widget

(def (component e) toggle-button/widget (button/widget)
  ((pushed-in :type boolean)))

(def (macro e) toggle-button/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'toggle-button/widget ,@args :content ,(the-only-element content)))

(def render-xhtml toggle-button/widget
  ;; TODO: setup border based on pushed-in
  (with-render-style/abstract (-self-)
    (render-content-for -self-)))

;;;;;;
;;; Drop down button widget

;; TODO: add contents and what? is it really a button or what?
(def (component e) drop-down-button/widget (button/widget)
  ())

(def (macro e) drop-down-button/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'drop-down-button/widget ,@args :content ,(the-only-element content)))

(def render-xhtml drop-down-button/widget
  (with-render-style/abstract (-self-)
    (render-content-for -self-)))
