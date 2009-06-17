;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Columns mixin

(def (component ea) columns/mixin ()
  ((columns :type components)))

(def (layered-function e) render-columns (component)
  (:method ((self columns/mixin))
    (foreach #'render-component (columns-of self))))

;;;;;;
;;; Column headers mixin

(def (component ea) column-headers/mixin ()
  ((column-headers :type components)))

(def (layered-function e) render-column-headers (component)
  (:method ((self column-headers/mixin))
    (foreach #'render-component (column-headers-of self))))

;;;;;;
;;; Column header abstract

(def (component ea) column-header/abstract ()
  ((cell-factory :type (or null function))))

;;;;;;
;;; Column header basic

(def (component ea) column-header/basic (column-header/abstract content/mixin)
  ())

(def (macro e) column ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'column-header/basic ,@args :content ,(the-only-element content)))

(def render-xhtml column-header/basic
  (with-render-style/abstract (-self- :element-name "th")
    (call-next-method)))

(def render-ods column-header/basic
  <table:table-cell ,(call-next-method)>)

;;;;;;
;;; Column abstract

(def (component ea) column/abstract ()
  ())

(def method supports-debug-component-hierarchy? ((self column/abstract))
  #f)

;;;;;;
;;; Column basic

;; TODO:
(def (component ea) column/basic (column/abstract style/abstract cells/mixin)
  ())

;;;;;;
;;; Entire column basic

;; TODO:
(def (component ea) entire-column/basic (column/abstract style/abstract content/mixin)
  ())
