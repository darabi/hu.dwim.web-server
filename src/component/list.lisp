;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;;;
;;; List

(def component list-component (style-component)
  ((orientation :vertical :type (member :vertical :horizontal))
   (components nil :type components)))

(def function render-list (orientation components &key id css-class style)
  (check-type orientation (member :vertical :horizontal))
  <table (:class `str("list " ,(ecase orientation
                                      (:vertical "vertical ")
                                      (:horizontal "horizontal "))
                              ,css-class)
          :id ,id
          :style ,style)
    ,@(ecase orientation
             (:vertical (mapcar (lambda (element)
                                  (when (force (visible-p element))
                                    <tr <td ,(render element)>>))
                                components))
             (:horizontal (list <tr ,(foreach (lambda (element)
                                                (when (force (visible-p element))
                                                  <td ,(render element)>))
                                              components)>)))>)

(def (function e) render-vertical-list (components &key id css-class style)
  (render-list :vertical components :id id :css-class css-class :style style))

(def (function e) render-horizontal-list (components &key id css-class style)
  (render-list :horizontal components :id id :css-class css-class :style style))

(def render list-component ()
  (bind (((:read-only-slots orientation components id css-class style) -self-))
    (render-list orientation components :id id :css-class css-class :style style)))

(def method refresh-component ((self list-component))
  (foreach #'mark-outdated (components-of self)))

;;;;;;
;;; Horizontal list

(def component horizontal-list-component (list-component)
  ()
  (:default-initargs :orientation :horizontal))

(def (function e) make-horizontal-list-component (&rest components)
  (make-instance 'horizontal-list-component :components components))

(def (macro e) horizontal-list ((&rest args &key &allow-other-keys) &body components)
  `(make-instance 'horizontal-list-component ,@args :components (remove nil (list ,@components))))

;;;;;;
;;; Vertical list

(def component vertical-list-component (list-component)
  ()
  (:default-initargs :orientation :vertical))

(def (function e) make-vertical-list-component (&rest components)
  (make-instance 'vertical-list-component :components components))

(def (macro e) vertical-list ((&rest args &key &allow-other-keys) &body components)
  `(make-instance 'vertical-list-component ,@args :components (remove nil (list ,@components))))
