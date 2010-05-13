;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; pivot-sheet-table/widget

(def (component e) pivot-sheet-table/widget (extended-table/widget)
  ())

(def function pivot-sheet-table-cell? (component)
  (typep (parent-component-of component) 'pivot-sheet-table/widget))

;;;;;;
;;; Pivot table

(def layer pivot-table-layer ()
  ())

(def (component e) pivot-table/widget (content/mixin)
  ((sheet-axes nil :type (components pivot-table/widget))
   (row-axes nil :type (components pivot-table/widget))
   (column-axes nil :type (components pivot-table/widget))
   (cell-axes nil :type (components pivot-table/widget))))

(def layered-method refresh-component :before ((self pivot-table/widget))
  (bind (((:slots sheet-axes row-axes row-headers column-axes column-headers cell-axes header-cell instances content) self))
    (with-active-layers (pivot-table-layer)
      (labels ((make-content-presentation (axes sheet-path)
                 (if axes
                     (bind ((axis (first axes))
                            (categories (categories-of (ensure-refreshed axis))))
                       (if categories
                           (make-instance 'tab-container-component
                                          :pages (mapcar [make-instance 'tab-page-component
                                                                        :header !1
                                                                        :content (make-content-presentation (rest axes) (cons !1 sheet-path))]
                                                         categories))
                           (make-pivot-sheet-table-component self sheet-path)))
                     (make-pivot-sheet-table-component self sheet-path))))
        (setf content (make-content-presentation sheet-axes nil))))))

(def generic make-pivot-sheet-table-component (component sheet-path)
  (:method ((self pivot-table/widget) sheet-path)
    (bind (((:slots row-axes column-axes) self))
      (labels ((make-axes-headers (axes &optional path)
                 (unless (null axes)
                   (bind ((axis (first axes)))
                     (ensure-refreshed axis)
                     (mapcar (lambda (category)
                               (make-instance 'header/widget
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
        (make-instance 'pivot-sheet-table/widget
                       :row-headers (make-axes-headers row-axes)
                       :column-headers (make-axes-headers column-axes)
                       :header-cell (make-viewer self :initial-alternative-type 't/reference/presentation))))))

(def layered-method make-title :in pivot-table-layer :around ((self component))
  (unless (pivot-sheet-table-cell? self)
    (call-next-layered-method)))

(def method make-reference-label ((reference reference-component) (class component-class) (component pivot-table/widget))
  (localized-class-name class :capitalize-first-letter #t))

(def layered-method collect-standard-object-detail-inspector-slots ((component standard-object-detail-inspector) (class component-class) (instance pivot-table/widget))
  (filter-slots '(sheet-axes row-axes column-axes cell-axes) (call-next-layered-method)))

(def layered-method make-context-menu-items ((component standard-object-inspector) (class component-class) (prototype pivot-table/widget) (instance pivot-table/widget))
  nil)

(def layered-method make-command-bar-commands :in pivot-table-layer :around ((self component) class prototype value)
  (unless (pivot-sheet-table-cell? self)
    (call-next-layered-method)))

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
      (command/widget ()
        (find-icon icon)
        (make-action
          ;; TODO: revive
          (remove-place (make-component-place axis))
          (appendf (slot-value pivot-table slot-name) (list axis))
          (mark-to-be-refreshed-component pivot-table))))))

;;;;;;
;;; Icon

(def (icon e) move-to-sheet-axes)

(def (icon e) move-to-row-axes)

(def (icon e) move-to-column-axes)

(def (icon e) move-to-cell-axes)

;;;;;;
;;; Pivot table axis component

(def (component e) pivot-table-axis/widget ()
  ((categories nil :type component)))

(def render-xhtml pivot-table-axis/widget
  <span ,(foreach #'render-component (categories-of -self-))>)

(def generic localized-pivot-table-axis (component))

(def method make-reference-label ((reference reference-component) (class component-class) (component pivot-table-axis/widget))
  (localized-pivot-table-axis component))

(def layered-method make-context-menu-items ((component standard-object-list-inspector) (class component-class) (prototype pivot-table-axis/widget) (instance pivot-table-axis/widget))
  nil)

(def layered-method make-context-menu-items ((component standard-object-row-inspector) (class component-class) (prototype pivot-table-axis/widget) (instance pivot-table-axis/widget))
  (optional-list (make-move-backward-command component)
                 (make-move-forward-command component)
                 (make-move-to-sheet-axes-command component)
                 (make-move-to-row-axes-command component)
                 (make-move-to-column-axes-command component)
                 (make-move-to-cell-axes-command component)))

(def layered-method collect-standard-object-list-table-inspector-slots ((component standard-object-list-table-inspector) (class component-class) (instance pivot-table-axis/widget))
  nil)

;;;;;
;;; Pivot table category component

(def (component e) pivot-table-category/widget (content/mixin)
  ())
