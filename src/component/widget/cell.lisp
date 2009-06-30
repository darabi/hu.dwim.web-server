;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Cell widget

(def (component e) cell/widget (widget/basic style/abstract content/mixin)
  ((column-span nil :type integer)
   (row-span nil :type integer)
   (word-wrap :type boolean)
   (horizontal-alignment nil :type (member nil :left :center :right))
   (vertical-alignment nil :type (member nil :top :center :bottom))))

(def with-macro* render-cell/widget (cell &key css-class)
  (setf css-class (ensure-list css-class))
  (if (typep cell 'cell/widget)
      (bind (((:read-only-slots column-span row-span horizontal-alignment vertical-alignment) cell))
        (ecase horizontal-alignment
          (:right (push +table-cell-horizontal-alignment-css-class/right+ css-class))
          (:center (push +table-cell-horizontal-alignment-css-class/center+ css-class))
          ((:left nil) nil))
        (ecase vertical-alignment
          (:top (push +table-cell-vertical-alignment-css-class/top+ css-class))
          (:bottom (push +table-cell-vertical-alignment-css-class/bottom+ css-class))
          ((:center nil) nil))
        (when (slot-boundp cell 'word-wrap)
          (ecase (slot-value cell 'word-wrap)
            ((#f) (push +table-cell-nowrap-css-class+ css-class))
            ((#t) nil)))
        <td (:class ,(join-strings css-class)
             :colspan ,column-span
             :rowspan ,row-span)
            ,(-body-)>)
      <td (:class ,(join-strings css-class))
        ,(-body-)>))

(def render-xhtml cell/widget
  (render-cell/widget (-self-)
    (call-next-method)))

(def render-ods cell/widget
  <table:table-cell ,(call-next-method)>)
