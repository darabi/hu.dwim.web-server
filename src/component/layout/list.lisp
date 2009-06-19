;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; List abstract

(def (component e) list/abstract (container/abstract orientation/mixin)
  ((orientation :vertical :type (member :vertical :horizontal))))

;;;;;;
;;; List basic

(def (component e) list/layout (container/layout style/abstract)
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

(def render-xhtml list/layout
  (bind (((:read-only-slots orientation contents id css-class style) -self-))
    (render-list orientation contents :id id :css-class css-class :style style)))

(def refresh-component list/layout
  (foreach #'mark-to-be-refreshed-component (contents-of -self-)))

;;;;;;
;;; Horizontal list basic

(def (component e) horizontal-list/layout (list/layout)
  ()
  (:default-initargs :orientation :horizontal))

(def (function e) make-horizontal-list-component (&rest contents)
  (make-instance 'horizontal-list/layout :contents contents))

(def (macro e) horizontal-list ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'horizontal-list/layout ,@args :contents (optional-list ,@contents)))

;;;;;;
;;; Vertical list basic

(def (component e) vertical-list/layout (list/layout)
  ()
  (:default-initargs :orientation :vertical))

(def (function e) make-vertical-list-component (&rest contents)
  (make-instance 'vertical-list/layout :contents contents))

(def (macro e) vertical-list ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'vertical-list/layout ,@args :contents (optional-list ,@contents)))
