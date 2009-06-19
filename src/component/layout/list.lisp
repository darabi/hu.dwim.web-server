;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;;;
;;; Orientation mixin

(def (component e) orientation/mixin ()
  ((orientation :vertical :type (member :vertical :horizontal))))

;;;;;;;;
;;; List abstract

(def (component e) list/abstract (container/abstract orientation/mixin)
  ())

;;;;;;;;
;;; List basic

(def (component e) list/basic (container/basic style/abstract)
  ((orientation :vertical :type (member :vertical :horizontal))))

(def function render-list (orientation contents &key id css-class style)
  (check-type orientation (member :vertical :horizontal))
  <table (:class `str("list " ,(ecase orientation
                                      (:vertical "vertical ")
                                      (:horizontal "horizontal "))
                              ,css-class)
          :id ,id
          :style ,style)
    <tbody ,@(ecase orientation
                    (:vertical (mapcar (lambda (element)
                                         (when (visible-component? element)
                                           <tr <td ,(render-component element)>>))
                                       contents))
                    (:horizontal (list <tr ,(foreach (lambda (element)
                                                       (when (visible-component? element)
                                                         <td ,(render-component element)>))
                                                     contents)>)))>>)

(def (function e) render-vertical-list (contents &key id css-class style)
  (render-list :vertical contents :id id :css-class css-class :style style))

(def (function e) render-horizontal-list (contents &key id css-class style)
  (render-list :horizontal contents :id id :css-class css-class :style style))

(def render-xhtml list/basic
  (bind (((:read-only-slots orientation contents id css-class style) -self-))
    (render-list orientation contents :id id :css-class css-class :style style)))

(def refresh-component list/basic
  (foreach #'mark-component-to-be-refreshed (contents-of -self-)))

;;;;;;
;;; Horizontal list basic

(def (component e) horizontal-list/basic (list/basic)
  ()
  (:default-initargs :orientation :horizontal))

(def (function e) make-horizontal-list-component (&rest contents)
  (make-instance 'horizontal-list/basic :contents contents))

(def (macro e) horizontal-list ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'horizontal-list/basic ,@args :contents (optional-list ,@contents)))

;;;;;;
;;; Vertical list basic

(def (component e) vertical-list/basic (list/basic)
  ()
  (:default-initargs :orientation :vertical))

(def (function e) make-vertical-list-component (&rest contents)
  (make-instance 'vertical-list/basic :contents contents))

(def (macro e) vertical-list ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'vertical-list/basic ,@args :contents (optional-list ,@contents)))
