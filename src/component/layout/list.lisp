;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; List layout

(def (component e) list/layout (layout/minimal contents/abstract)
  ((orientation :vertical :type (member :vertical :horizontal)))
  (:documentation "Either a vertical or a horizontal list LAYOUT."))

(def (macro e) list/layout ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'list/layout ,@args :contents (list ,@contents)))

(def render-xhtml list/layout
  (render-list-layout (orientation-of -self-) (contents-of -self-)))

(def function render-list-layout (orientation contents)
  (check-type orientation (member :vertical :horizontal))
  <table (:class `str("list layout " ,(ecase orientation
                                        (:vertical "vertical")
                                        (:horizontal "horizontal"))))
    <tbody ,(ecase orientation
              (:vertical (foreach (lambda (element)
                                    (when (visible-component? element)
                                      <tr <td ,(render-component element)>>))
                                  contents))
              (:horizontal <tr ,(foreach (lambda (element)
                                           (when (visible-component? element)
                                             <td ,(render-component element)>))
                                         contents)>))>>)

(def (function e) render-vertical-list-layout (contents)
  (render-list-layout :vertical contents))

(def (function e) render-horizontal-list-layout (contents)
  (render-list-layout :horizontal contents))

;;;;;;
;;; Horizontal list layout

(def (component e) horizontal-list/layout (list/layout)
  ()
  (:default-initargs :orientation :horizontal)
  (:documentation "A horizontal list LAYOUT."))

(def (macro e) horizontal-list/layout ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'horizontal-list/layout ,@args :contents (optional-list ,@contents)))

;;;;;;
;;; Vertical list layout

(def (component e) vertical-list/layout (list/layout)
  ()
  (:default-initargs :orientation :vertical)
  (:documentation "A vertical list LAYOUT."))

(def (macro e) vertical-list/layout ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'vertical-list/layout ,@args :contents (optional-list ,@contents)))
