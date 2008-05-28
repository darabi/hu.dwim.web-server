;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icon

(def component icon-component ()
  ((name)
   (label nil)
   (image-url nil)
   (tooltip nil)))

(def render icon-component ()
  (with-slots (name label image-url tooltip) self
    <span (:title ,(or (force tooltip) ""))
      ,@(when image-url (list <img (:src ,image-url)>))
      ,@(when label (list (force label)))>))

(def (macro e) make-icon-component (name &rest args)
  `(make-instance 'icon-component :name ,name ,@args))

(def special-variable *icons* (make-hash-table))

(def function lookup-icon (name)
  (gethash name *icons*))

(def definer icon (name image-url)
  (bind ((name-as-string (string-downcase name)))
    `(setf (gethash ',name *icons*)
           (make-icon-component ',name
                                :image-url ,image-url
                                :label (delay (lookup-resource ,(format nil "icon-label.~A" name-as-string) nil))
                                :tooltip (delay (lookup-resource ,(format nil "icon-tooltip.~A" name-as-string) nil))))))

(def icon refresh "static/wui/icons/20x20/ying-yang-arrows.png")
(defresources hu
  (icon-label.refresh "Frissítés")
  (icon-tooltip.refresh "A tartalom frissítése"))
(defresources en
  (icon-label.refresh "Refresh")
  (icon-tooltip.refresh "Refresh content"))

(def icon edit "static/wui/icons/20x20/pen-on-document.png")
(defresources hu
  (icon-label.edit "Szerkesztés")
  (icon-tooltip.edit "Szerkesztés elkezdése"))
(defresources en
  (icon-label.edit "Edit")
  (icon-tooltip.edit "Start editing"))

(def icon save "static/wui/icons/20x20/disc-on-document.png")
(defresources hu
  (icon-label.save "Mentés")
  (icon-tooltip.save "A változtatások mentése"))
(defresources en
  (icon-label.save "Save")
  (icon-tooltip.save "Save changes"))

(def icon cancel "static/wui/icons/20x20/yellow-x.png")
(defresources hu
  (icon-label.cancel "Elvetés")
  (icon-tooltip.cancel "Változtatások elvetése"))
(defresources en
  (icon-label.cancel "Cancel")
  (icon-tooltip.cancel "Revert changes"))

(def icon delete "static/wui/icons/20x20/red-x.png")
(defresources hu
  (icon-label.delete "Törlés")
  (icon-tooltip.delete "Az objektum törlése"))
(defresources en
  (icon-label.delete "Delete")
  (icon-tooltip.delete "Delete object"))
