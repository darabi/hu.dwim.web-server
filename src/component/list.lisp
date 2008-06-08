;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;;;
;;; List

(def component list-component (style-component-mixin value-component)
  ((elements nil)
   (orientation :vertical :type (member :vertical :horizontal))
   (components nil :type components)))

(def method component-value-of ((component list-component))
  (elements-of component))

(def method (setf component-value-of) (new-value (component list-component))
  (with-slots (elements components) component
    (setf elements new-value)
    (unless components
      (setf components (mapcar (lambda (element)
                                 (make-viewer-component element :default-component-type 'reference-component))
                               new-value)))))

(def function render-list (orientation elements &key id css-class style)
  (check-type orientation (member :vertical :horizontal))
  <table (:class `str("list " ,(ecase orientation
                                      (:vertical "vertical ")
                                      (:horizontal "horizontal "))
                              ,@(ensure-list css-class))
          :id ,id
          :style ,style)
    ,@(ecase orientation
             (:vertical (mapcar (lambda (element)
                                  <tr <td ,(render element)>>)
                                elements))
             (:horizontal (list <tr ,@(mapcar (lambda (element)
                                                <td ,(render element)>)
                                              elements) >)))>)

(def function render-vertical-list (elements &key id css-class style)
  (render-list :vertical elements :id id :css-class css-class :style style))

(def function render-horizontal-list (elements &key id css-class style)
  (render-list :horizontal elements :id id :css-class css-class :style style))

(def render list-component ()
  (with-slots (orientation components id css-class style) -self-
    (render-list orientation components :id id :css-class css-class :style style)))

;;;;;;
;;; Horizontal list

(def component horizontal-list-component (list-component)
  ()
  (:default-initargs :orientation :horizontal))

(def (function e) make-horizontal-list-component (&rest components)
  (make-instance 'horizontal-list-component :components components))

(def (macro e) horizontal-list ((&rest args &key &allow-other-keys) &body components)
  `(make-instance 'horizontal-list-component ,@args :components (list ,@components)))

;;;;;;
;;; Vertical list

(def component vertical-list-component (list-component)
  ()
  (:default-initargs :orientation :vertical))

(def (function e) make-vertical-list-component (&rest components)
  (make-instance 'vertical-list-component :components components))

(def (macro e) vertical-list ((&rest args &key &allow-other-keys) &body components)
  `(make-instance 'vertical-list-component ,@args :components (list ,@components)))
