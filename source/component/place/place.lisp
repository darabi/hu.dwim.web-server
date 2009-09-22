;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place/presentation

(def (component e) place/presentation (t/presentation)
  ())

;;;;;;;
;;; place/reference/presentation

(def (component e) place/reference/presentation (t/reference/presentation)
  ())

(def layered-method make-reference/content ((component place/reference/presentation) class prototype (value place))
  (format nil "~A: ~A" (capitalize-first-letter (substitute #\Space #\- (string-downcase (symbol-name (class-name class))))) (place-name value)))

;;;;;;
;;; place/value/presentation

(def (component e) place/value/presentation (t/detail/presentation content/widget)
  ())

(def refresh-component place/value/presentation
  (bind (((:slots component-value content) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (if content
        (setf (component-value-of content) (value-at-place component-value))
        (setf content (make-slot-value/content -self- class prototype component-value)))))

(def (layered-function e) make-slot-value/content (component class prototype value))
