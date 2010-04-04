;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; target-place/widget

(def (component e) target-place/widget (widget/basic content/abstract)
  ((target-place :type place))
  (:documentation "A TARGET-PLACE/WIDGET has a PLACE that refers into its CONTENT. This place can be set by REPLACE-TARGET-PLACE/WIDGET descendant COMPONENTs."))

(def (macro e) target-place/widget ((&rest args &key target-place &allow-other-keys) &body content)
  ;; evaluation of target-place must be after content
  (remove-from-plistf args :target-place)
  `(make-instance 'target-place/widget ,@args
                  :content ,(the-only-element content)
                  :target-place ,target-place))

(def render-xhtml target-place/widget
  <div (:class "target-place widget")
    ,(render-content-for -self-)>)

;;;;;;
;;; replace-target-place/widget

;; TODO rename to replace-target-place/command/widget?
(def (component e) replace-target-place/widget (command/widget)
  ((replacement-component :type t))
  (:documentation "A REPLACE-TARGET-PLACE/WIDGET is a COMMAND/WIDGET that will replace the TARGET-PLAGE of its nearest TARGET-PLACE/WIDGET ancestor."))

(def (macro e) replace-target-place/widget ((&rest args &key &allow-other-keys) content &body forms)
  `(make-instance 'replace-target-place/widget ,@args
                  :content ,content
                  :replacement-component (one-time-delay ,@forms)))

(def constructor replace-target-place/widget
  (setf (action-of -self-) (make-component-action -self-
                             (replace-target-place -self- (component-dispatch-class -self-) (component-dispatch-prototype -self-) (component-value-of -self-)))))

(def refresh-component replace-target-place/widget
  (bind (((:slots ajax) -self-))
    (when ajax
      (bind ((target-place (target-place-of (find-replace-target-place-widget -self-)))
             (component (component-at-place target-place)))
        (setf ajax (if (typep component 'parent/mixin)
                       (awhen (find-ancestor-component-with-type component 'id/mixin :otherwise #f)
                         (ajax-of it))
                       #t))))))

(def function render-replace-target-place-command/xhtml (component replacement-component content &rest args &key &allow-other-keys)
  (apply 'render-command/xhtml (make-component-action component
                                 (bind ((target-place (target-place-of (find-replace-target-place-widget component))))
                                   (setf (component-at-place target-place) replacement-component)))
         content args))

(def function find-replace-target-place-widget (component)
  (find-ancestor-component component
                           (lambda (ancestor)
                             (and (typep ancestor 'target-place/widget)
                                  (target-place-of ancestor)))))

(def (generic e) replace-target-place (component class prototype value)
  (:method ((component replace-target-place/widget) class prototype value)
    (bind ((target-place (target-place-of (find-replace-target-place-widget component)))
           (replacement-component (force (replacement-component-of component))))
      (setf (component-at-place target-place) replacement-component))))
