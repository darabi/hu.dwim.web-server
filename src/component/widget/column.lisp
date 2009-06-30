;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Column header widget

(def (component e) column-header/widget (widget/basic content/mixin)
  ((cell-factory :type (or null function)))
  (:documentation "A COLUMN-HEADER is special CELL in the header HEADER-ROW of each COLUMN of a TABLE."))

(def (macro e) column-header/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'column-header/widget ,@args :content ,(the-only-element content)))

(def render-xhtml column-header/widget
  (with-render-style/abstract (-self- :element-name "th")
    (call-next-method)))

(def render-ods column-header/widget
  <table:table-cell ,(call-next-method)>)

;;;;;;
;;; Column widget

(def (component e) column/widget (widget/basic style/abstract cells/mixin)
  ())

(def method supports-debug-component-hierarchy? ((self column/widget))
  #f)

;;;;;;
;;; Entire column widget

;; TODO:
(def (component e) entire-column/widget (column/abstract style/abstract content/mixin)
  ())
