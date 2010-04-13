;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; about/widget

;; TODO: this needs to be revisited, make it more parametrizable, not just by overriding render, etc.

(def (component e) about/widget (widget/style title/mixin)
  ()
  (:default-initargs :title (title/widget () "About")))

(def (macro e) about/widget (&rest args &key &allow-other-keys)
  `(make-instance 'about/widget ,@args))

(def render-xhtml about/widget
  (with-render-style/abstract (-self-)
    (render-title-for -self-)
    (render-about/dwim)
    (render-about/thellminar)))

(def method component-style-class ((self about/widget))
  (string+ "content-border " (call-next-method)))

(def (function e) render-about/contributors (id title url image-url image-alt people)
  (bind ((box-id (string+ id "-about-box"))
         (title-id (string+ box-id "-title")))
    <h1 ,title>
    <table <tr <td <a (:href ,url :target "_blank")
                      <img (:src ,image-url :alt ,image-alt)>>>
               <td (:class ,+table-cell-vertical-alignment-style-class/center+)
                   ,(iter (for person :in-sequence people)
                          <div ,person>)>>>))

(def (function e) render-about/dwim ()
  (render-about/contributors "dwim" "Szoftver" "http://dwim.hu"
                             "/static/wui/image/about/dwim-logo.png" "DWIM"
                             (list "Lendvai Attila"
                                   "Mészáros Levente"
                                   "Borbély Tamás"
                                   "Mészáros Bálint")))

(def (function e) render-about/thellminar ()
  (render-about/contributors "thellminar" "Stílus" "http://www.thellminar.hu"
                             "/static/wui/image/about/thellminar-logo.png" "ThellMinar"
                             (list "Páka Tamás")))
