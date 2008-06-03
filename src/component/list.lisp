;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;;;
;;; List

(def function render-list (orientation elements &key id css-class style)
  (check-type orientation (member :vertical :horizontal))
  <div (:class `str("list " ,(if (eq orientation :vertical)
                                 "vertical "
                                 "horizontal ")
                            ,@(ensure-list css-class))
        :id ,id
        :style ,style)
    ,@(render-list-elements elements)>)

(def function render-vertical-list (elements)
  (render-list :vertical elements))

(def function render-horizontal-list (elements)
  (render-list :horizontal elements))

(def function render-list-elements (elements)
  (iter (for element :in elements)
        (for index :upfrom 1)
        ;; TODO this will be broken for ajax in this setup
        (collect <div (:class ,(if (evenp index) "even" "odd"))
                   ,(render element)>)))

(def component list-component (widget-component-mixin value-component)
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

(def render list-component ()
  (with-slots (orientation components id css-class style) -self-
    (render-list orientation components :id id :css-class css-class :style style)))

(def component horizontal-list-component (list-component)
  ()
  (:default-initargs :orientation :horizontal))

(def component vertical-list-component (list-component)
  ()
  (:default-initargs :orientation :vertical))
