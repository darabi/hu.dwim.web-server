;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icon

(def component icon-component ()
  ((name)
   (label nil :export :accessor)
   (image-path nil :export :accessor)
   (tooltip nil :export :accessor)))

(def render icon-component ()
  (with-slots (label image-path tooltip) -self-
    <span (:title ,(or (force tooltip) ""))
     ,@(when image-path
             (list <img (:src ,(concatenate-string (path-prefix-of *application*) image-path))>))
     ,@(when label
             (list (force label)))>))

(def (macro e) icon (name &rest args)
  `(make-instance 'icon-component :name ,name ,@args))

(def (macro e) make-icon-component (name &rest args)
  `(make-instance 'icon-component :name ,name ,@args))

(def special-variable *icons* (make-hash-table))

(def (function e) find-icon (name)
  (prog1-bind icon (gethash name *icons*)
    (unless icon
      (error "The icon ~A cannot be found" name))))

(def function (setf find-icon) (icon name)
  (setf (gethash name *icons*) icon))

(def (definer e) icon (name image-path)
  (bind ((name-as-string (string-downcase name)))
    `(setf (find-icon ',name)
           (make-icon-component ',name
                                :image-path ,image-path
                                :label (delay (lookup-resource ,(format nil "icon-label.~A" name-as-string) nil))
                                :tooltip (delay (lookup-resource ,(format nil "icon-tooltip.~A" name-as-string) nil))))))

(def (function e) clone-icon (name &rest args)
  (bind ((icon (find-icon name)))
    (apply #'make-instance 'icon-component :name name (append args (list :label (label-of icon) :image-path (image-path-of icon) :tooltip (tooltip-of icon))))))

;;;;;;
;;; Default icons

(def icon login "static/wui/icons/20x20/green-checkmark.png")
(defresources hu
  (icon-label.login "Belépés")
  (icon-tooltip.login "Azonosítás és jogosultságok kérése"))
(defresources en
  (icon-label.login "Login")
  (icon-tooltip.login "Gain privileges by authentication"))

(def icon logout "static/wui/icons/20x20/red-arrow-on-door.png")
(defresources hu
  (icon-label.logout "Kilépés")
  (icon-tooltip.logout "A munka befejezése és a jogosultságok feladása"))
(defresources en
  (icon-label.logout "Logout")
  (icon-tooltip.logout "Leave the current session and remove all privileges previously gained by authentication"))

(def icon impersonalization "static/wui/icons/20x20/vcr-play-with-people.png")
(defresources hu
  (icon-label.impersonalization "Megszemélyesítés")
  (icon-tooltip.impersonalization "A kiválasztott alany megszemélyesítése"))
(defresources en
  (icon-label.impersonalization "Impersonalization")
  (icon-tooltip.impersonalization "Impersonalize the selected subject"))

(def icon cancel-impersonalization "static/wui/icons/20x20/vcr-backward-with-people.png")
(defresources hu
  (icon-label.cancel-impersonalization "Megszemélyesítés megszüntetése")
  (icon-tooltip.cancel-impersonalization "Visszatérés az eredeti alanyhoz"))
(defresources en
  (icon-label.cancel-impersonalization "Cancel impersonalization")
  (icon-tooltip.cancel-impersonalization "Return back to the original subject"))

(def icon change-password "static/wui/icons/20x20/green-checkmark.png")
(defresources hu
  (icon-label.change-password "Jelszó megváltoztatása")
  (icon-tooltip.change-password "A munka befejezése és a jogosultságok feladása"))
(defresources en
  (icon-label.change-password "Change password")
  (icon-tooltip.change-password "Leave the current session and remove all privileges previously gained by authentication"))

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
  (icon-tooltip.save "Változtatások mentése"))
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

(def icon new "static/wui/icons/20x20/document.png")
(defresources hu
  (icon-label.new "Új")
  (icon-tooltip.new "Új objektum elkezdése"))
(defresources en
  (icon-label.new "New")
  (icon-tooltip.new "Start editing new object"))

(def icon create "static/wui/icons/20x20/disc-on-document.png")
(defresources hu
  (icon-label.create "Létrehozás")
  (icon-tooltip.create "Új objektum felvétele"))
(defresources en
  (icon-label.create "Create")
  (icon-tooltip.create "Create object"))

(def icon delete "static/wui/icons/20x20/red-x.png")
(defresources hu
  (icon-label.delete "Törlés")
  (icon-tooltip.delete "Az objektum törlése"))
(defresources en
  (icon-label.delete "Delete")
  (icon-tooltip.delete "Delete object"))

(def icon top "static/wui/icons/20x20/blue-all-direction-arrows.png")
(defresources hu
  (icon-label.top "Tetejére")
  (icon-tooltip.top "A lap tetejére"))
(defresources en
  (icon-label.top "Top")
  (icon-tooltip.top "Move to top"))

(def icon back "static/wui/icons/20x20/green-double-left-arrow.png")
(defresources hu
  (icon-label.back "Vissza")
  (icon-tooltip.back "Vissza a helyére"))
(defresources en
  (icon-label.back "Back")
  (icon-tooltip.back "Move back"))

(def icon expand "static/wui/icons/20x20/magnifier-plus.png")
(defresources hu
  (icon-label.expand "Kinyitás")
  (icon-tooltip.expand "Részletek megjelenítése"))
(defresources en
  (icon-label.expand "Expand")
  (icon-tooltip.expand "Expand to detail"))

(def icon collapse "static/wui/icons/20x20/magnifier-minus.png")
(defresources hu
  (icon-label.collapse "Összecsukás")
  (icon-tooltip.collapse "Részletek elrejtése"))
(defresources en
  (icon-label.collapse "Collapse")
  (icon-tooltip.collapse "Collapse to reference"))

(def icon first "static/wui/icons/20x20/vcr-begin.png")
(defresources hu
  (icon-label.first "Első")
  (icon-tooltip.first "Ugrás az első lapra"))
(defresources en
  (icon-label.first "First")
  (icon-tooltip.first "Jump to first page"))

(def icon previous "static/wui/icons/20x20/vcr-backward.png")
(defresources hu
  (icon-label.previous "Előző")
  (icon-tooltip.previous "Lapozás a előző lapra"))
(defresources en
  (icon-label.previous "Previous")
  (icon-tooltip.previous "Move to previous page"))

(def icon next "static/wui/icons/20x20/vcr-forward.png")
(defresources hu
  (icon-label.next "Következő")
  (icon-tooltip.next "Lapozás a következő lapra"))
(defresources en
  (icon-label.next "Next")
  (icon-tooltip.next "Move to next page"))

(def icon last "static/wui/icons/20x20/vcr-end.png")
(defresources hu
  (icon-label.last "Utolsó")
  (icon-tooltip.last "Ugrás az utolsó lapra"))
(defresources en
  (icon-label.last "Last")
  (icon-tooltip.last "Jump to last page"))

(def icon filter "static/wui/icons/20x20/binocular.png")
(defresources hu
  (icon-label.filter "Keresés")
  (icon-tooltip.filter "A keresés végrehajtása"))
(defresources en
  (icon-label.filter "Filter")
  (icon-tooltip.filter "Execute the filter"))

(def icon equal "static/wui/icons/20x20/equal-sign.png")
(defresources hu
  (icon-label.equal "Egyenlő")
  (icon-tooltip.equal "Ellenőrzes egyenlőségre"))
(defresources en
  (icon-label.equal "Equal")
  (icon-tooltip.equal "Compare for equality"))

(def icon like "static/wui/icons/20x20/tilde.png")
(defresources hu
  (icon-label.like "Hasonló")
  (icon-tooltip.like "Ellenőrzes hasonlóságra"))
(defresources en
  (icon-label.like "Like")
  (icon-tooltip.like "Compare for like"))

(def icon negated "static/wui/icons/20x20/no-entry.png") ;; TODO: find better icon
(defresources hu
  (icon-label.negated "Negált")
  (icon-tooltip.negated "Negált feltétel"))
(defresources en
  (icon-label.negated "Negated")
  (icon-tooltip.negated "Negate condition"))

(def icon ponated "static/wui/icons/20x20/checkmark.png") ;; TODO: find better icon
(defresources hu
  (icon-label.ponated "Ponált")
  (icon-tooltip.ponated "Ponált feltétel"))
(defresources en
  (icon-label.ponated "Ponated")
  (icon-tooltip.ponated "Ponate condition"))

(def icon view "static/wui/icons/20x20/eye.png")
(defresources hu
  (icon-label.view "Nézet")
  (icon-tooltip.view "Nézet váltás"))
(defresources en
  (icon-label.view "View")
  (icon-tooltip.view "Change view"))

(def icon finish "static/wui/icons/20x20/checkmark.png")
(defresources hu
  (icon-label.finish "Befejezés")
  (icon-tooltip.finish "A varázsló befejezése"))
(defresources en
  (icon-label.finish "Finish")
  (icon-tooltip.finish "Finish wizard"))
