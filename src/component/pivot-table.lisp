;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Pivot table

;; TODO: what about the css classes in render? axis, header, tooltip, etc. how do we put those in place?
(def component pivot-table-component (extended-table-component)
  ((row-axes nil :type components)
   (column-axes nil :type components)
   (cell-axes nil :type components) ;; TODO: implement
   (cells nil :type components)))

(def method refresh-component :before ((self pivot-table-component))
  (with-slots (row-axes row-headers column-axes column-headers instances) self
    (labels ((swap (axes p1 p2)
               (bind ((tmp (elt axes p1)))
                 (setf (elt axes p1) (elt axes p2))
                 (setf (elt axes p2) tmp)
                 axes))
             (axes-headers (axes &optional path)
               (unless (null axes)
                 (mapcar (lambda (category)
                           (make-instance 'table-header-component
                                          :content (clone-component (content-of category))
                                          :children (axes-headers (rest axes) (cons category path))
                                          :expanded (find-if (lambda (instance)
                                                               (every (lambda (c)
                                                                        (funcall (predicate-of c) instance))
                                                                      (cons category path)))
                                                             instances)))
                         (categories-of (first axes)))))
             (axes-command-bars (axes primary-axes-slot-name secondary-axes-slot-name)
               (bind ((row-axes? (eq primary-axes-slot-name 'row-axes)))
                 (unless (null axes)
                   (list (make-instance 'table-header-component
                                        :content (make-instance 'command-bar-component
                                                                :commands (bind ((axis (first axes))
                                                                                 (primary-axes (slot-value self primary-axes-slot-name))
                                                                                 (secondary-axes (slot-value self secondary-axes-slot-name))
                                                                                 (position (position axis primary-axes)))
                                                                            (optional-list (command (if row-axes?
                                                                                                        (icon rotate-clockwise :label nil)
                                                                                                        (icon rotate-counter-clockwise :label nil))
                                                                                                    (make-action
                                                                                                      (setf (slot-value self primary-axes-slot-name) (remove axis primary-axes))
                                                                                                      (setf (slot-value self secondary-axes-slot-name) (cons axis secondary-axes))
                                                                                                      (refresh-component self)))
                                                                                           (unless (zerop position)
                                                                                             (command (if row-axes?
                                                                                                          (icon move-left :label nil)
                                                                                                          (icon move-up :label nil))
                                                                                                      (make-action
                                                                                                        ;; TODO: this impl is really stupid
                                                                                                        (setf (slot-value self primary-axes-slot-name) (swap primary-axes position (1- position)))
                                                                                                        (refresh-component self))))
                                                                                           (unless (= position
                                                                                                      (1- (length primary-axes)))
                                                                                             (command (if row-axes?
                                                                                                          (icon move-right :label nil)
                                                                                                          (icon move-down :label nil))
                                                                                                      (make-action
                                                                                                        ;; TODO: this impl is really ugly
                                                                                                        (setf (slot-value self primary-axes-slot-name) (swap primary-axes position (1+ position)))
                                                                                                        (refresh-component self)))))))
                                        :children (axes-command-bars (rest axes) primary-axes-slot-name secondary-axes-slot-name)))))))
      (setf row-headers (append (axes-command-bars row-axes 'row-axes 'column-axes)
                                (axes-headers row-axes)))
      (setf column-headers (append (axes-command-bars column-axes 'column-axes 'row-axes)
                                   (axes-headers column-axes))))))

(def icon rotate-clockwise)
(def resources hu
  (icon-tooltip.rotate-clockwise "Elforgatás a másik tengelyre"))
(def resources en
  (icon-tooltip.rotate-clockwise "Rotate to other axis"))

(def icon rotate-counter-clockwise)
(def resources hu
  (icon-tooltip.rotate-counter-clockwise "Elforgatás a másik tengelyre"))
(def resources en
  (icon-tooltip.rotate-counter-clockwise "Rotate to other axis"))

(def icon move-up)
(def resources hu
  (icon-tooltip.move-up "Mozgatás felfelé"))
(def resources en
  (icon-tooltip.move-up "Move up"))

(def icon move-down)
(def resources hu
  (icon-tooltip.move-down "Mozgatás lefelé"))
(def resources en
  (icon-tooltip.move-down "Move down"))

(def icon move-left)
(def resources hu
  (icon-tooltip.move-left "Mozgatás balra"))
(def resources en
  (icon-tooltip.move-left "Move left"))

(def icon move-right)
(def resources hu
  (icon-tooltip.move-right "Mozgatás jobbra"))
(def resources en
  (icon-tooltip.move-right "Move right"))

;;;;;;
;;; Pivot table axis

(def component pivot-table-axis-component ()
  ((categories nil :type component)))

;;;;;
;;; Pivot table category

(def component pivot-table-category-component (content-component)
  ())
