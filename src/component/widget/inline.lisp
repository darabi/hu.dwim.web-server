;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Thunk mixin

(def (component e) thunk/mixin ()
  ((thunk :type (or symbol function)))
  (:documentation "A COMPONENT with a FUNCTION."))

;;;;;;
;;; Inline render component widget

(def (component e) inline-render-component/widget (widget/basic thunk/mixin)
  ()
  (:documentation "An INLINE-RENDER-COMPONENT/WIDGET has a FUNCTION that is called in its RENDER-COMPONENT."))

(def (macro e) inline-render-component/widget ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 'inline-render-component/widget ,@args :thunk (lambda () ,@forms)))

(def function render-inline-render-component (component)
  (funcall (thunk-of component)))

(def render-component inline-render-component/widget
  (render-inline-render-component -self-))

;;;;;;
;;; Inline render xhtml widget

(def (component e) inline-render-xhtml/widget (inline-render-component/widget)
  ()
  (:documentation "An INLINE-RENDER-XHTML/WIDGET can only be rendered in XHTML format."))

(def (macro e) inline-render-xhtml/widget ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 'inline-render-xhtml/widget ,@args :thunk (lambda () ,@forms)))

(def render-component inline-render-xhtml/widget
  (error "Cannot render ~A in the current format" -self-))

(def render-xhtml inline-render-xhtml/widget
  (render-inline-render-component -self-))

;;;;;;
;;; Wrap render component widget

(def (component e) wrap-render-component/widget (widget/basic content/abstract thunk/mixin)
  ()
  (:documentation "A WRAP-RENDER-COMPONENT/WIDGET has a FUNCTION and another COMPONENT inside. It wraps the rendering of its CONTENT with the rendering FUNCTION. The CONTENT can be rendered by calling the local function (-body-)."))

(def (macro e) wrap-render-component/widget ((&rest args &key &allow-other-keys) content &body forms)
  `(%wrap-render-component/widget ,(list* :class-name 'wrap-render-component/widget args) ,content ,@forms))

(def macro %wrap-render-component/widget ((&rest args &key class-name &allow-other-keys) content &body forms)
  (remove-from-plistf args :class-name)
  `(make-instance ',class-name ,@args
                  :thunk (lambda (next-method)
                           (flet ((-body- ()
                                    (funcall next-method)))
                             ,@forms))
                  :content ,content))

(def function render-wrap-render-component (component)
  (funcall (thunk-of component) (lambda () (render-content-for component))))

(def render-component wrap-render-component/widget
  (render-wrap-render-component -self-))

;;;;;;
;;; Wrap render XHTML widget

(def (component e) wrap-render-xhtml/widget (wrap-render-component/widget)
  ()
  (:documentation "An WRAP-RENDER-XHTML/WIDGET can only be rendered in XHTML format."))

(def (macro e) wrap-render-xhtml/widget ((&rest args &key &allow-other-keys) content &body forms)
  `(%wrap-render-component/widget ,(list* :class-name 'wrap-render-xhtml/widget args) ,content ,@forms))

(def render-component wrap-render-xhtml/widget
  (error "Cannot render ~A in non XHTML format" -self-))

(def render-xhtml wrap-render-xhtml/widget
  (render-wrap-render-component -self-))
