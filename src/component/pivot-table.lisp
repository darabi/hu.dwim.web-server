;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Pivot table

(def component pivot-table-component (content-component)
  ((sheet-axes nil :type (polimorph-list pivot-table-axis-component))
   (row-axes nil :type (polimorph-list pivot-table-axis-component))
   (column-axes nil :type (polimorph-list pivot-table-axis-component))
   (cell-axes nil :type (polimorph-list pivot-table-axis-component))))

(def method refresh-component :before ((self pivot-table-component))
  (bind (((:slots sheet-axes row-axes row-headers column-axes column-headers cell-axes header-cell instances content) self))
    (labels ((make-content (axes sheet-path)
               (if axes
                   (bind ((axis (first axes))
                          (categories (categories-of (ensure-uptodate axis))))
                     (if categories
                         (make-instance 'tab-container-component
                                        :pages (mapcar [make-instance 'tab-page-component
                                                                      :header !1
                                                                      :content (make-content (rest axes) (cons !1 sheet-path))]
                                                       categories))
                         (make-pivot-table-extended-table-component self sheet-path)))
                   (make-pivot-table-extended-table-component self sheet-path))))
      (setf content (make-content sheet-axes nil)))))

(def generic make-pivot-table-extended-table-component (component sheet-path)
  (:method ((self pivot-table-component) sheet-path)
    (bind (((:slots row-axes column-axes) self))
      (labels ((make-axes-headers (axes &optional path)
                 (unless (null axes)
                   (bind ((axis (first axes)))
                     (ensure-uptodate axis)
                     (mapcar (lambda (category)
                               (make-instance 'table-header-component
                                              :content (clone-component (content-of category))
                                              :children (make-axes-headers (rest axes) (cons category path))
                                              ;; TODO: instances is not a slot in the abstract pivot-table
                                              #+nil :expanded #+nil
                                              (find-if (lambda (instance)
                                                         (every (lambda (c)
                                                                  (funcall (predicate-of c) instance))
                                                                (cons category path)))
                                                       instances)))
                             (categories-of axis))))))
        (make-instance 'extended-table-component
                       :row-headers (make-axes-headers row-axes)
                       :column-headers (make-axes-headers column-axes)
                       :header-cell (make-viewer self :default-alternative-type 'reference-component))))))

(def method make-reference-label ((reference reference-component) (class component-class) (component pivot-table-component))
  (localized-class-name class :capitalize-first-letter #t))

(def layered-method collect-standard-object-detail-inspector-slots ((component standard-object-detail-inspector) (class component-class) (instance pivot-table-component))
  (filter-slots '(sheet-axes row-axes column-axes cell-axes) (call-next-method)))

(def layered-method make-standard-commands ((component standard-object-inspector) (class component-class) (prototype-or-instance pivot-table-component))
  nil)

(def icon move-to-sheet-axes)
(def resources hu
  (icon-label.move-to-sheet-axes "Lap tengely")
  (icon-tooltip.move-to-sheet-axes "Mozgatás a lap tengelyek közé"))
(def resources en
  (icon-label.move-to-sheet-axes "Sheet axis")
  (icon-tooltip.move-to-sheet-axes "Move to sheet axes"))

(def icon move-to-row-axes)
(def resources hu
  (icon-label.move-to-row-axes "Sor tengely")
  (icon-tooltip.move-to-row-axes "Mozgatás a sor tengelyek közé"))
(def resources en
  (icon-label.move-to-row-axes "Row axis")
  (icon-tooltip.move-to-row-axes "Move to row axes"))

(def icon move-to-column-axes)
(def resources hu
  (icon-label.move-to-column-axes "Oszlop tengely")
  (icon-tooltip.move-to-column-axes "Mozgatás a oszlop tengelyek közé"))
(def resources en
  (icon-label.move-to-column-axes "Column axis")
  (icon-tooltip.move-to-column-axes "Move to column axes"))

(def icon move-to-cell-axes)
(def resources hu
  (icon-label.move-to-cell-axes "Mező tengely")
  (icon-tooltip.move-to-cell-axes "Mozgatás a mező tengelyek közé"))
(def resources en
  (icon-label.move-to-cell-axes "Cell axis")
  (icon-tooltip.move-to-cell-axes "Move to cell axes"))

(def function make-move-to-sheet-axes-command (component)
  (make-move-to-axes-command component 'move-to-sheet-axes 'sheet-axes))

(def function make-move-to-row-axes-command (component)
  (make-move-to-axes-command component 'move-to-row-axes 'row-axes))

(def function make-move-to-column-axes-command (component)
  (make-move-to-axes-command component 'move-to-column-axes 'column-axes))

(def function make-move-to-cell-axes-command (component)
  (make-move-to-axes-command component 'move-to-cell-axes 'cell-axes))

(def function make-move-to-axes-command (component icon slot-name)
  (bind ((axis (instance-of component))
         (pivot-table (parent-component-of axis)))
    (unless (find axis (slot-value (parent-component-of axis) slot-name))
      (command (find-icon icon)
               (make-action
                 (remove-place (make-component-place axis))
                 (appendf (slot-value pivot-table slot-name) (list axis))
                 (mark-outdated pivot-table))))))

;;;;;;
;;; Pivot table axis component

(def component pivot-table-axis-component ()
  ((categories nil :type component)))

(def render pivot-table-axis-component
  <span ,(foreach #'render (categories-of -self-))>)

(def generic localized-pivot-table-axis (component))

(def method make-reference-label ((reference reference-component) (class component-class) (component pivot-table-axis-component))
  (localized-pivot-table-axis component))

(def layered-method make-standard-commands ((component standard-object-list-inspector) (class component-class) (prototype-or-instance pivot-table-axis-component))
  nil)

(def layered-method make-standard-commands ((component standard-object-row-inspector) (class component-class) (prototype-or-instance pivot-table-axis-component))
  (optional-list (make-move-backward-command component)
                 (make-move-forward-command component)
                 (make-move-to-sheet-axes-command component)
                 (make-move-to-row-axes-command component)
                 (make-move-to-column-axes-command component)
                 (make-move-to-cell-axes-command component)))

(def layered-method collect-standard-object-list-table-inspector-slots ((component standard-object-list-table-inspector) (class component-class) (instance pivot-table-axis-component))
  nil)

;;;;;
;;; Pivot table category component

(def component pivot-table-category-component (content-component)
  ())

;;;;;;
;;; Localization

(def resources hu
  (class-name.pivot-table-axis-component "pivot tábla tengely")

  (slot-name.sheet-axes "lap tengelyek")
  (slot-name.row-axes "sor tengelyek")
  (slot-name.column-axes "oszlop tengelyek")
  (slot-name.cell-axes "mező tengelyek"))
