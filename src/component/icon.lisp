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
  (bind (((:read-only-slots name label image-path tooltip) -self-))
    (render-icon -self- image-path :name name :label label :tooltip tooltip)))

(def render-csv icon-component ()
  (render-csv (force (label-of -self-))))

(def render-pdf icon-component ()
  (render-pdf (force (label-of -self-))))

(def layered-function render-icon-label (icon label)
  (:method (icon label)
    `xml,label))

(def (function e) render-icon (icon image-path &key name label tooltip)
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
          ,(if image-path
               <img (:src ,(concatenate-string (path-prefix-of *application*) image-path))>
               <img (:class ,(concatenate-string "icon " (string-downcase (symbol-name name)) "-icon"))>)
          ,(awhen (force label)
             (render-icon-label icon it)) >))

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

;; TODO: KLUDGE: do we really need an equal hash table here?
;; TODO: this was added because there are lookups with keys concatenated and there's no way currently
;; TODO: to find out the correct package, see member-component
(def special-variable *icons* (make-hash-table :test #'equal))

(def (function e) find-icon (name &key (otherwise `(:error "The icon ~A cannot be found" ,name)))
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

(def icon login)
(def resources hu
  (icon-label.login "Belépés")
  (icon-tooltip.login "Azonosítás és jogosultságok kérése"))
(def resources en
  (icon-label.login "Login")
  (icon-tooltip.login "Gain privileges by authentication"))

(def icon logout)
(def resources hu
  (icon-label.logout "Kilépés")
  (icon-tooltip.logout "A munka befejezése és a jogosultságok feladása"))
(def resources en
  (icon-label.logout "Logout")
  (icon-tooltip.logout "Leave the current session and remove all privileges previously gained by authentication"))

(def icon impersonalize)
(def resources hu
  (icon-label.impersonalize "Megszemélyesítés")
  (icon-tooltip.impersonalize "A kiválasztott alany megszemélyesítése"))
(def resources en
  (icon-label.impersonalize "Impersonalization")
  (icon-tooltip.impersonalize "Impersonalize the selected subject"))

(def icon cancel-impersonalization)
(def resources hu
  (icon-label.cancel-impersonalization "Megszemélyesítés megszüntetése")
  (icon-tooltip.cancel-impersonalization "Visszatérés az eredeti alanyhoz"))
(def resources en
  (icon-label.cancel-impersonalization "Cancel impersonalization")
  (icon-tooltip.cancel-impersonalization "Return back to the original subject"))

(def icon change-password)
(def resources hu
  (icon-label.change-password "Jelszó megváltoztatása")
  (icon-tooltip.change-password "A munka befejezése és a jogosultságok feladása"))
(def resources en
  (icon-label.change-password "Change password")
  (icon-tooltip.change-password "Leave the current session and remove all privileges previously gained by authentication"))

(def icon refresh)
(def resources hu
  (icon-label.refresh "Frissítés")
  (icon-tooltip.refresh "A tartalom frissítése"))
(def resources en
  (icon-label.refresh "Refresh")
  (icon-tooltip.refresh "Refresh content"))

(def icon edit)
(def resources hu
  (icon-label.edit "Szerkesztés")
  (icon-tooltip.edit "Szerkesztés elkezdése"))
(def resources en
  (icon-label.edit "Edit")
  (icon-tooltip.edit "Start editing"))

(def icon save)
(def resources hu
  (icon-label.save "Mentés")
  (icon-tooltip.save "Változtatások mentése és a szerkesztés befejezése"))
(def resources en
  (icon-label.save "Save")
  (icon-tooltip.save "Save changes and finish editing"))

(def icon cancel)
(def resources hu
  (icon-label.cancel "Elvetés")
  (icon-tooltip.cancel "Változtatások elvetése és a szerkesztés befejezése"))
(def resources en
  (icon-label.cancel "Cancel")
  (icon-tooltip.cancel "Cancel changes and finish editing"))

(def icon store)
(def resources hu
  (icon-label.store "Mentés")
  (icon-tooltip.store "Változtatások mentése"))
(def resources en
  (icon-label.store "Store")
  (icon-tooltip.store "Store changes"))

(def icon revert)
(def resources hu
  (icon-label.revert "Elvetés")
  (icon-tooltip.revert "Változtatások elvetése"))
(def resources en
  (icon-label.revert "Revert")
  (icon-tooltip.revert "Revert changes"))

(def icon new)
(def resources hu
  (icon-label.new "Új")
  (icon-tooltip.new "Új objektum szerkesztése"))
(def resources en
  (icon-label.new "New")
  (icon-tooltip.new "Start editing a new object"))

(def icon create)
(def resources hu
  (icon-label.create "Létrehozás")
  (icon-tooltip.create "Új objektum felvétele"))
(def resources en
  (icon-label.create "Create")
  (icon-tooltip.create "Create object"))

(def icon delete)
(def resources hu
  (icon-label.delete "Törlés")
  (icon-tooltip.delete "Az objektum törlése"))
(def resources en
  (icon-label.delete "Delete")
  (icon-tooltip.delete "Delete object"))

(def icon close)
(def resources hu
  (icon-label.close "Bezárás")
  (icon-tooltip.close "A komponens bezárása"))
(def resources en
  (icon-label.close "Close")
  (icon-tooltip.close "Close the component"))

(def icon focus)
(def resources hu
  (icon-label.focus "Fókuszálás")
  (icon-tooltip.focus "Fókuszálás az objektumra"))
(def resources en
  (icon-label.focus "Focus")
  (icon-tooltip.focus "Focus on the object"))

(def icon open-in-new-frame)
(def resources hu
  (icon-label.open-in-new-frame "Új ablak")
  (icon-tooltip.open-in-new-frame "Az objektum új ablakban való megnyitása"))
(def resources en
  (icon-label.open-in-new-frame "New window")
  (icon-tooltip.open-in-new-frame "Open object in new window"))

(def icon back)
(def resources hu
  (icon-label.back "Vissza")
  (icon-tooltip.back "Vissza a helyére"))
(def resources en
  (icon-label.back "Back")
  (icon-tooltip.back "Move back"))

(def icon expand)
(def resources hu
  (icon-label.expand "Kinyitás")
  (icon-tooltip.expand "Részletek megjelenítése"))
(def resources en
  (icon-label.expand "Expand")
  (icon-tooltip.expand "Expand to detail"))

(def icon collapse)
(def resources hu
  (icon-label.collapse "Összecsukás")
  (icon-tooltip.collapse "Részletek elrejtése"))
(def resources en
  (icon-label.collapse "Collapse")
  (icon-tooltip.collapse "Collapse to reference"))

(def icon first)
(def resources hu
  (icon-label.first "Első")
  (icon-tooltip.first "Ugrás az első lapra"))
(def resources en
  (icon-label.first "First")
  (icon-tooltip.first "Jump to first page"))

(def icon previous)
(def resources hu
  (icon-label.previous "Előző")
  (icon-tooltip.previous "Lapozás a előző lapra"))
(def resources en
  (icon-label.previous "Previous")
  (icon-tooltip.previous "Move to previous page"))

(def icon next)
(def resources hu
  (icon-label.next "Következő")
  (icon-tooltip.next "Lapozás a következő lapra"))
(def resources en
  (icon-label.next "Next")
  (icon-tooltip.next "Move to next page"))

(def icon last)
(def resources hu
  (icon-label.last "Utolsó")
  (icon-tooltip.last "Ugrás az utolsó lapra"))
(def resources en
  (icon-label.last "Last")
  (icon-tooltip.last "Jump to last page"))

(def icon filter)
(def resources hu
  (icon-label.filter "Keresés")
  (icon-tooltip.filter "A keresés végrehajtása"))
(def resources en
  (icon-label.filter "Filter")
  (icon-tooltip.filter "Execute the filter"))

(def icon find)
(def resources hu
  (icon-label.find "Keresés")
  (icon-tooltip.find "Egy objektum keresése"))
(def resources en
  (icon-label.find "Find")
  (icon-tooltip.find "Find an object"))

(def icon set-to-nil)
(def resources hu
  (icon-label.set-to-nil "Szétkapcsolás")
  (icon-tooltip.set-to-nil "Az objektumok szétkapcsolása"))
(def resources en
  (icon-label.set-to-nil "Disconnect")
  (icon-tooltip.set-to-nil "Disconnect from object"))

(def icon set-to-unbound)
(def resources hu
  (icon-label.set-to-unbound "Alapértelmezett")
  (icon-tooltip.set-to-unbound "Az alapértelmezett értékre beállítása"))
(def resources en
  (icon-label.set-to-unbound "Default")
  (icon-tooltip.set-to-unbound "Set to default"))

(def icon select)
(def resources hu
  (icon-label.select "Kiválasztás")
  (icon-tooltip.select "Egy objektum kiválasztása"))
(def resources en
  (icon-label.select "Select")
  (icon-tooltip.select "Select an object"))

(def icon equal)
(def resources hu
  (icon-label.equal "Egyenlő")
  (icon-tooltip.equal "Ellenőrzes egyenlőségre"))
(def resources en
  (icon-label.equal "Equal")
  (icon-tooltip.equal "Compare for equality"))

(def icon like)
(def resources hu
  (icon-label.like "Hasonló")
  (icon-tooltip.like "Ellenőrzes hasonlóságra"))
(def resources en
  (icon-label.like "Like")
  (icon-tooltip.like "Compare for like"))

(def icon <)
(def resources hu
  (icon-label.< "Kisebb")
  (icon-tooltip.< "Ellenőrzes kisebbre"))
(def resources en
  (icon-label.< "Less")
  (icon-tooltip.< "Compare for less then"))

(def icon <=)
(def resources hu
  (icon-label.<= "Kisebb vagy egyenlő")
  (icon-tooltip.<= "Ellenőrzes kisebbre vagy egyenlőre"))
(def resources en
  (icon-label.<= "Less or equal")
  (icon-tooltip.<= "Compare for less than or equal"))

(def icon >)
(def resources hu
  (icon-label.> "Nagyobb")
  (icon-tooltip.> "Ellenőrzes nagyobbra"))
(def resources en
  (icon-label.> "Greater")
  (icon-tooltip.> "Compare for greater then"))

(def icon >=)
(def resources hu
  (icon-label.>= "Nagyobb vagy egyenlő")
  (icon-tooltip.>= "Ellenőrzes nagyobb vagy egyenlőre"))
(def resources en
  (icon-label.>= "Greater or equal")
  (icon-tooltip.>= "Compare for greater than or equal"))

(def icon negated)
(def resources hu
  (icon-label.negated "Negált")
  (icon-tooltip.negated "Negált feltétel"))
(def resources en
  (icon-label.negated "Negated")
  (icon-tooltip.negated "Negate condition"))

(def icon ponated)
(def resources hu
  (icon-label.ponated "Ponált")
  (icon-tooltip.ponated "Ponált feltétel"))
(def resources en
  (icon-label.ponated "Ponated")
  (icon-tooltip.ponated "Ponate condition"))

(def icon view)
(def resources hu
  (icon-label.view "Nézet")
  (icon-tooltip.view "Nézet váltás"))
(def resources en
  (icon-label.view "View")
  (icon-tooltip.view "Change view"))

(def icon finish)
(def resources hu
  (icon-label.finish "Befejezés")
  (icon-tooltip.finish "A varázsló befejezése"))
(def resources en
  (icon-label.finish "Finish")
  (icon-tooltip.finish "Finish wizard"))

(def icon download)
(def resources hu
  (icon-label.download "Letöltés")
  (icon-tooltip.download "Fájl letöltése"))
(def resources en
  (icon-label.download "Download")
  (icon-tooltip.download "Download file"))

(def icon upload)
(def resources hu
  (icon-label.upload "Feltöltés")
  (icon-tooltip.upload "Fájl feltöltése"))
(def resources en
  (icon-label.upload "Upload")
  (icon-tooltip.upload "Upload file"))

(def icon diagram)
(def resources hu
  (icon-label.diagram "Ábra")
  (icon-tooltip.diagram "Ábra megjelenítése"))
(def resources en
  (icon-label.diagram "Diagram")
  (icon-tooltip.diagram "Show diagram"))
