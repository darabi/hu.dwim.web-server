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
  (bind (((:read-only-slots label image-path tooltip) -self-))
    (render-icon -self- image-path :label label :tooltip tooltip)))

(def layered-function render-icon-label (icon label)
  (:method (icon label)
    `xml,label))

(def function render-icon (icon image-path &key label tooltip)
  (bind ((tooltip (force tooltip))
         (delayed-content-tooltip? (and tooltip
                                        (not (stringp tooltip))))
         (id (when delayed-content-tooltip?
               (generate-frame-unique-string))))
    ;; render the `js first, so the return value contract of qq is kept.
    (when delayed-content-tooltip?
      ;; TODO this could be collected and emitted using a (map ... data) to spare some space
      `js(on-load
          (new dojox.widget.DynamicTooltip
               (create :connectId (array ,id)
                       :position (array "below" "right")
                       :href ,(etypecase tooltip
                                (action (register-action/href tooltip :delayed-content #t))
                                (uri (print-uri-to-string tooltip)))))))
    <span (:id ,id :title ,(unless delayed-content-tooltip? tooltip))
          ,@(when image-path
                  (list <img (:src ,(concatenate-string (path-prefix-of *application*) image-path))> " "))
          ,(when label
                 (render-icon-label icon (force label)))>))

(def (macro e) icon (name &rest args)
  `(make-icon-component ',name ,@args))

(def (function e) make-icon-component (name &rest args)
  (bind ((icon (find-icon name :otherwise nil)))
    (if icon
        (apply #'make-instance 'icon-component
               :name name (append args
                                  (list :label (label-of icon)
                                        :image-path (image-path-of icon)
                                        :tooltip (tooltip-of icon))))
        (if args
            (apply #'make-instance 'icon-component :name name args)
            (error "The icon ~A cannot be found and no arguments were specified" name)))))

(def special-variable *icons* (make-hash-table))

(def (function e) find-icon (name &key (otherwise '(:error "The icon ~A cannot be found" name)))
  (prog1-bind icon (gethash name *icons*)
    (unless icon
      (handle-otherwise otherwise))))

(def function (setf find-icon) (icon name)
  (setf (gethash name *icons*) icon))

(def (definer e) icon (name image-path &key (label nil label-p) (tooltip nil tooltip-p))
  (bind ((name-as-string (string-downcase name)))
    `(setf (find-icon ',name)
           (make-instance 'icon-component
                          :name ',name
                          :image-path ,image-path
                          :label ,(if label-p
                                      label
                                      `(delay (lookup-resource ,(concatenate-string "icon-label." name-as-string))))
                          :tooltip ,(if tooltip-p
                                        tooltip
                                        `(delay (lookup-resource ,(concatenate-string "icon-tooltip." name-as-string))))))))

;;;;;;
;;; Default icons
;;; TODO: move the icons where they are used?

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
  (icon-tooltip.save "Változtatások mentése és a szerkesztés befejezése"))
(defresources en
  (icon-label.save "Save")
  (icon-tooltip.save "Save changes and finish editing"))

(def icon cancel "static/wui/icons/20x20/yellow-x.png")
(defresources hu
  (icon-label.cancel "Elvetés")
  (icon-tooltip.cancel "Változtatások elvetése és a szerkesztés befejezése"))
(defresources en
  (icon-label.cancel "Cancel")
  (icon-tooltip.cancel "Cancel changes and finish editing"))

(def icon store "static/wui/icons/20x20/disc-on-document.png")
(defresources hu
  (icon-label.store "Mentés")
  (icon-tooltip.store "Változtatások mentése"))
(defresources en
  (icon-label.store "Store")
  (icon-tooltip.store "Store changes"))

(def icon revert "static/wui/icons/20x20/yellow-x.png")
(defresources hu
  (icon-label.revert "Elvetés")
  (icon-tooltip.revert "Változtatások elvetése"))
(defresources en
  (icon-label.revert "Revert")
  (icon-tooltip.revert "Revert changes"))

(def icon new "static/wui/icons/20x20/document.png")
(defresources hu
  (icon-label.new "Új")
  (icon-tooltip.new "Új objektum szerkesztése"))
(defresources en
  (icon-label.new "New")
  (icon-tooltip.new "Start editing a new object"))

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

(def icon close "static/wui/icons/20x20/red-x.png")
(defresources hu
  (icon-label.close "Bezárás")
  (icon-tooltip.close "A komponens bezárása"))
(defresources en
  (icon-label.close "Close")
  (icon-tooltip.close "Close the component"))

(def icon top "static/wui/icons/20x20/blue-all-direction-arrows.png")
(defresources hu
  (icon-label.top "Tetejére")
  (icon-tooltip.top "A lap tetejére"))
(defresources en
  (icon-label.top "Top")
  (icon-tooltip.top "Move to top"))

(def icon open-in-new-frame "static/wui/icons/20x20/split-vertically.png")
(defresources hu
  (icon-label.open-in-new-frame "Ablak")
  (icon-tooltip.open-in-new-frame "Az objektum új ablakban való megnyitása"))
(defresources en
  (icon-label.open-in-new-frame "Frame")
  (icon-tooltip.open-in-new-frame "Open object in new frame"))

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

(def icon find "static/wui/icons/20x20/binocular.png")
(defresources hu
  (icon-label.find "Keresés")
  (icon-tooltip.find "Egy objektum keresése"))
(defresources en
  (icon-label.find "Find")
  (icon-tooltip.find "Find an object"))

(def icon set-to-nil "static/wui/icons/20x20/unplug.png")
(defresources hu
  (icon-label.set-to-nil "Szétkapcsolás")
  (icon-tooltip.set-to-nil "Az objektumok szétkapcsolása"))
(defresources en
  (icon-label.set-to-nil "Disconnect")
  (icon-tooltip.set-to-nil "Disconnect from object"))

(def icon set-to-unbound "static/wui/icons/20x20/unplug.png")
(defresources hu
  (icon-label.set-to-unbound "Alapértelmezett")
  (icon-tooltip.set-to-unbound "Az alapértelmezett értékre beállítása"))
(defresources en
  (icon-label.set-to-unbound "Default")
  (icon-tooltip.set-to-unbound "Set to default"))

(def icon select "static/wui/icons/20x20/checkmark.png")
(defresources hu
  (icon-label.select "Kiválasztás")
  (icon-tooltip.select "Egy objektum kiválasztása"))
(defresources en
  (icon-label.select "Select")
  (icon-tooltip.select "Select an object"))

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

(def icon < "static/wui/icons/20x20/less-than-sign.png")
(defresources hu
  (icon-label.< "Kisebb")
  (icon-tooltip.< "Ellenőrzes kisebbre"))
(defresources en
  (icon-label.< "Less")
  (icon-tooltip.< "Compare for less then"))

(def icon <= "static/wui/icons/20x20/less-than-or-equal-sign.png")
(defresources hu
  (icon-label.<= "Kisebb vagy egyenlő")
  (icon-tooltip.<= "Ellenőrzes kisebbre vagy egyenlőre"))
(defresources en
  (icon-label.<= "Less or equal")
  (icon-tooltip.<= "Compare for less than or equal"))

(def icon > "static/wui/icons/20x20/greater-than-sign.png")
(defresources hu
  (icon-label.> "Nagyobb")
  (icon-tooltip.> "Ellenőrzes nagyobbra"))
(defresources en
  (icon-label.> "Greater")
  (icon-tooltip.> "Compare for greater then"))

(def icon >= "static/wui/icons/20x20/greater-than-or-equal-sign.png")
(defresources hu
  (icon-label.>= "Nagyobb vagy egyenlő")
  (icon-tooltip.>= "Ellenőrzes nagyobb vagy egyenlőre"))
(defresources en
  (icon-label.>= "Greater or equal")
  (icon-tooltip.>= "Compare for greater than or equal"))

(def icon negated "static/wui/icons/20x20/thumb-down.png")
(defresources hu
  (icon-label.negated "Negált")
  (icon-tooltip.negated "Negált feltétel"))
(defresources en
  (icon-label.negated "Negated")
  (icon-tooltip.negated "Negate condition"))

(def icon ponated "static/wui/icons/20x20/thumb-up.png")
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

(def icon download "static/wui/icons/20x20/file-download.png")
(defresources hu
  (icon-label.download "Letöltés")
  (icon-tooltip.download "Fájl letöltése"))
(defresources en
  (icon-label.download "Download")
  (icon-tooltip.download "Download file"))

(def icon upload "static/wui/icons/20x20/file-upload.png")
(defresources hu
  (icon-label.upload "Feltöltés")
  (icon-tooltip.upload "Fájl feltöltése"))
(defresources en
  (icon-label.upload "Upload")
  (icon-tooltip.upload "Upload file"))
