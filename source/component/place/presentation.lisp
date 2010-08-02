;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place/presentation

(def (component e) place/presentation (t/presentation)
  ()
  (:documentation "Presentation for reified places."))

;;;;;;
;;; place/alternator/presentation

(def (component e) place/alternator/presentation (place/presentation t/alternator/presentation)
  ()
  (:documentation "Presentation for reified places."))

;;;;;;;
;;; place/reference/presentation

(def (component e) place/reference/presentation (place/presentation t/reference/presentation)
  ())

(def layered-method make-reference-content ((component place/reference/presentation) class prototype (value place))
  (format nil "~A: ~A" (capitalize-first-letter (substitute #\Space #\- (string-downcase (symbol-name (class-name class))))) (place-name value)))

;;;;;;;
;;; place/detail/presentation

(def (component e) place/detail/presentation (place/presentation t/detail/presentation)
  ())

;;;;;;
;;; place/value/presentation

(def (component e) place/value/presentation (place/detail/presentation content/widget)
  ())

(def refresh-component place/value/presentation
  (bind (((:slots component-value content) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (if content
        (setf (component-value-of content) (place-value component-value))
        (setf content (make-content-presentation -self- class prototype component-value)))))

(def render-xhtml place/value/presentation
  (with-render-style/component (-self- :element-name "span")
    (render-context-menu-for -self-)
    (render-content-for -self-)))
