;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icon

(def resources hu
  (icon-label.new "Új")
  (icon-tooltip.new "Új objektum szerkesztése")

  (icon-label.create "Létrehozás")
  (icon-tooltip.create "Új objektum felvétele")

  (icon-label.delete "Törlés")
  (icon-tooltip.delete "Az objektum törlése")

  (icon-label.close "Bezárás")
  (icon-tooltip.close "A komponens bezárása")

  (icon-label.back "Vissza")
  (icon-tooltip.back "Vissza a helyére")

  (icon-label.expand-component nil)
  (icon-tooltip.expand-component "Részletek megjelenítése")

  (icon-label.collapse-component nil)
  (icon-tooltip.collapse-component "Részletek elrejtése")

  (icon-label.expand-from-reference "Kinyitás")
  (icon-tooltip.expand-from-reference "Részletek megjelenítése")

  (icon-label.collapse-to-reference "Összecsukás")
  (icon-tooltip.collapse-to-reference "Részletek elrejtése")

  (icon-label.filter "Keresés")
  (icon-tooltip.filter "A keresés végrehajtása")

  (icon-label.find "Keresés")
  (icon-tooltip.find "Egy objektum keresése")

  (icon-label.set-to-nil "Szétkapcsolás")
  (icon-tooltip.set-to-nil "Az objektumok szétkapcsolása")

  (icon-label.set-to-unbound "Alapértelmezett")
  (icon-tooltip.set-to-unbound "Az alapértelmezett értékre beállítása")

  (icon-label.select "Kiválasztás")
  (icon-tooltip.select "Egy objektum kiválasztása")

  (icon-label.view "Nézet")
  (icon-tooltip.view "Nézet váltás")

  (icon-label.finish "Befejezés")
  (icon-tooltip.finish "A varázsló befejezése")

  (icon-label.cancel "Mégse")
  (icon-tooltip.cancel "Az aktuális művelet félbeszakítása az esetleges változtatások elmentése nélkül"))

;;;;;;
;;; Error handling

(def resources hu
  (render-failed-to-load-page (&key &allow-other-keys)
    <div (:id ,+page-failed-to-load-id+)
     <h1 "Gyanúsan sokáig tart az oldal betöltése...">
     <p "Sajnos egyes böngészők néha összezavarodnak az egyébként hibátlan oldal betöltése közben is. Az "
        <a (:href "#" :onclick "return _wui_handleFailedToLoad()") "oldal újratöltése">
        ", esetleg a " <i "Frissítés">  " gomb használata a legtöbb esetben megoldja a problémát.">>))

;;;;;;
;;; Context sensitive help

(def resources hu
  (icon-label.help "Segítség")
  (help.no-context-sensitive-help-available "Nincs környezetfüggő segítség")
  (help.help-about-context-sensitive-help-button "Ez a környezetfüggő segítség üzemmódnak a ki- és bekapcsoló gombja. Segítség üzemmódban az egérrel megállva a képernyő különböző pontjain feljön egy hasonló buborék mint ez, ami megmutatja az adott pontra legrelevánsabb környezetfüggő segítséget (jelen esetben magának a segítség üzemmódnak a leírását). A segítség üzemmódot a kérdőjel formájú egér kurzor jelzi. Ilyenkor az egérrel bárhova kattintva a segítség üzemmód kikapcsol."))

;;;;;;
;;; File up/download

(def resources hu
  (file-last-modification-timestamp (file)
    `xml,"Frissítve: "
    (if (probe-file file)
        (localized-timestamp (local-time:universal-to-timestamp (file-write-date file)))
        <span (:class "missing-file")
              "Hiányzik a fájl!">))

  (icon-label.download "Letöltés")
  (icon-tooltip.download "Fájl letöltése")

  (icon-label.upload "Feltöltés")
  (icon-tooltip.upload "Fájl feltöltése"))

;;;;;;
;;; Interaction

(def resources hu
  (interaction-aborted "Kérem javítsa a megjelölt hibákat és próbálja újra a műveletet"))

;;;;;;
;;; Chart

(def resources hu
  (chart.missing-flash-plugin "A Flash Plugin nem elérhető"))

;;;;;;
;;; Diagram

(def resources hu
  (icon-label.diagram "Ábra")
  (icon-tooltip.diagram "Ábra megjelenítése"))

;;;;;;
;;; Cloneable

(def resources hu
  (icon-label.open-in-new-frame "Új ablak")
  (icon-tooltip.open-in-new-frame "Az objektum új ablakban való megnyitása"))

;;;;;;
;;; Editing

(def resources hu
  (icon-label.begin-editing "Szerkesztés")
  (icon-tooltip.begin-editing "Szerkesztés elkezdése")

  (icon-label.save-editing "Mentés")
  (icon-tooltip.save-editing "Változtatások mentése és a szerkesztés befejezése")

  (icon-label.cancel-editing "Elvetés")
  (icon-tooltip.cancel-editing "Változtatások elvetése és a szerkesztés befejezése")

  (icon-label.store-editing "Mentés")
  (icon-tooltip.store-editing "Változtatások mentése")

  (icon-label.revert-editing "Elvetés")
  (icon-tooltip.revert-editing "Változtatások elvetése"))

;;;;;;
;;; Export

(def resources hu
  (icon-label.export-text "Szöveg")
  (icon-tooltip.export-text "A tartalom mentése szöveges formátumban")

  (icon-label.export-csv "CSV")
  (icon-tooltip.export-csv "A tartalom mentése CSV formátumban")

  (icon-label.export-pdf "PDF")
  (icon-tooltip.export-pdf "A tartalom mentése PDF formátumban")

  (icon-label.export-odt "ODT")
  (icon-tooltip.export-odt "A tartalom mentése ODT formátumban")

  (icon-label.export-ods "ODS")
  (icon-tooltip.export-ods "A tartalom mentése ODS formátumban"))

;;;;;;
;;; Refreshable

(def resources hu
  (icon-label.refresh "Frissítés")
  (icon-tooltip.refresh "A tartalom frissítése"))

;;;;;;
;;; Filter

(def resources hu
  (no-matches-were-found "Nincs a keresésnek megfelelő objektum")

  (icon-label.equal "Egyenlő")
  (icon-tooltip.equal "Ellenőrzes egyenlőségre")

  (icon-label.like "Hasonló")
  (icon-tooltip.like "Ellenőrzes hasonlóságra")

  (icon-label.< "Kisebb")
  (icon-tooltip.< "Ellenőrzes kisebbre")

  (icon-label.<= "Kisebb vagy egyenlő")
  (icon-tooltip.<= "Ellenőrzes kisebbre vagy egyenlőre")

  (icon-label.> "Nagyobb")
  (icon-tooltip.> "Ellenőrzes nagyobbra")

  (icon-label.>= "Nagyobb vagy egyenlő")
  (icon-tooltip.>= "Ellenőrzes nagyobb vagy egyenlőre")

  (icon-label.negated "Negált")
  (icon-tooltip.negated "Negált feltétel")

  (icon-label.ponated "Ponált")
  (icon-tooltip.ponated "Ponált feltétel")

  (predicate.= "Egyenlő")
  (predicate.~ "Hasonló")
  (predicate.< "Kisebb")
  (predicate.≤ "Kisebb vagy egyenlő")
  (predicate.> "Nagyobb")
  (predicate.≥ "Nagyobb vagy egyenlő"))

;;;;;;
;;; Movable

(def resources hu
  (icon-label.move-backward "Mozgatás hátra")
  (icon-tooltip.move-backward "Objektum mozgatása hátra a listában"))

(def resources hu
  (icon-label.move-forward "Mozgatás előre")
  (icon-tooltip.move-forward "Objektum mozgatása előre a listában"))

;;;;;;
;;; Primitive

(def resources hu
  (value.default "alapértelmezett")
  (value.defaults-to "alapértelmezett érték: ")
  (value.unbound "alapértelmezett")
  (value.nil "nincs")

  (boolean.true "igaz")
  (boolean.false "hamis")

  (member-type-value.nil ""))

;;;;;;
;;; Authentication

(def resources hu
  (login.title "Belépés")
  (login.identifier "Azonosító")
  (login.password "Jelszó")
  (login.message.authentication-failed "Azonosítás sikertelen")
  (login.message.session-timed-out "Lejárt a biztonsági idő, kérem lépjen be újra")

  (icon-label.login "Belépés")
  (icon-tooltip.login "Azonosítás és jogosultságok kérése")

  (icon-label.logout "Kilépés")
  (icon-tooltip.logout "A munka befejezése és a jogosultságok feladása"))

;;;;;;
;;; Expression

(def resources hu
  (icon-label.add-expression-argument "Hozzáadás")
  (icon-tooltip.add-expression-argument "Új paraméter hozzáadása")

  (icon-label.remove-expression-argument "Törlés")
  (icon-tooltip.remove-expression-argument "Paraméter törlése"))

;;;;;;
;;; Menu

(def resources hu
  (icon-label.menu "Menü")
  (icon-tooltip.show-context-menu "Környezetfüggő menü megjelenítése")
  (context-menu.move-commands "Mozgatás"))

;;;;;;
;;; Page navigation

(def resources hu
  (icon-label.go-to-first-page "Első")
  (icon-tooltip.go-to-first-page "Ugrás az első lapra")

  (icon-label.go-to-previous-page "Előző")
  (icon-tooltip.go-to-previous-page "Lapozás a előző lapra")

  (icon-label.go-to-next-page "Következő")
  (icon-tooltip.go-to-next-page "Lapozás a következő lapra")

  (icon-label.go-to-last-page "Utolsó")
  (icon-tooltip.go-to-last-page "Ugrás az utolsó lapra")

  (page-size-selector.rows/page " sor/oldal"))

;;;;;;
;;; Pivot table

(def resources hu
  (icon-label.move-to-sheet-axes "Lap tengely")
  (icon-tooltip.move-to-sheet-axes "Mozgatás a lap tengelyek közé")

  (icon-label.move-to-row-axes "Sor tengely")
  (icon-tooltip.move-to-row-axes "Mozgatás a sor tengelyek közé")

  (icon-label.move-to-column-axes "Oszlop tengely")
  (icon-tooltip.move-to-column-axes "Mozgatás a oszlop tengelyek közé")

  (icon-label.move-to-cell-axes "Mező tengely")
  (icon-tooltip.move-to-cell-axes "Mozgatás a mező tengelyek közé")

  (class-name.pivot-table-axis-component "pivot tábla tengely")

  (slot-name.sheet-axes "lap tengelyek")
  (slot-name.row-axes "sor tengelyek")
  (slot-name.column-axes "oszlop tengelyek")
  (slot-name.cell-axes "mező tengelyek"))

;;;;;;
;;; Tab container

(def resources hu
  (icon-label.swith-to-tab-page "Lap")
  (icon-tooltip.swith-to-tab-page "A lap előrehozása"))

;;;;;;
;;; Top

(def resources hu
  (icon-label.focus-in "Fókuszálás")
  (icon-tooltip.focus-in "Fókuszálás az objektumra")

  (icon-label.focus-out "Vissza")
  (icon-tooltip.focus-out "Fókuszálás megszüntetése"))

;;;;;;
;;; Object

(def resources hu
  (class-name.standard-object "objektum")

  (standard-object-detail-component.primary-group "Elsődleges tulajdonságok")
  (standard-object-detail-component.secondary-group "Egyéb tulajdonságok")

  (standard-object-slot-value-group.there-are-no-slots "Nincs egy tulajdonság sem")

  (selectable-standard-object-tree-inspector.title (class-name)
    (concatenate-string "Egy " class-name " kiválasztása"))
  (standard-object-filter.title (class-name)
    (concatenate-string (capitalize-first-letter class-name) " keresése"))
  (standard-object-detail-filter.class-selector-label "Típus")
  (standard-object-detail-filter.ordering-specifier-label "Rendezés")
  (standard-object-list-inspector.title (class-name)
    (concatenate-string "Egy " class-name " lista megjelenítése"))
  (standard-object-tree-inspector.title (class-name)
    (concatenate-string "Egy " class-name " fa megjelenítése"))
  (standard-object-inspector.title (class-name)
    (concatenate-string "Egy " class-name " megjelenítése"))
  (standard-object-maker.title (class-name)
    (concatenate-string "Egy új " class-name " felvétele"))

  (object-list-table.column.commands "")
  (object-list-table.column.type "Típus")
  (object-tree-table.column.commands "")
  (object-tree-table.column.type "Típus")
  (standard-object-detail-maker.class-selector-label "Típus")

  (delete-instance.dialog.title "Objektum törlése")
  (delete-instance.dialog.body (&key instance &allow-other-keys)
    <p "Biztos benne, hogy le szeretné törölni a következő objektumot?"
       <br>
       ;; TODO replace with a reference renderer
       ,(princ-to-string instance)>))
