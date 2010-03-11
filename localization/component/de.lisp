;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icon

(def localization de
  (icon-label.new "Neu")
  (icon-tooltip.new "Ein neues Objekt anlegen und sofort bearbeiten")

  (icon-label.create "Objekt anlegen")
  (icon-tooltip.create "Ein neues Objekt anlegen")

  (icon-label.delete "Löschen")
  (icon-tooltip.delete "Ein Objekt löschen")

  (icon-label.close "Schließen")
  (icon-tooltip.close "Dieses Unterformular schließen")

  (icon-label.navigate-back "Zurück")
  (icon-tooltip.back "Einen Schritt zurück gehen")

  (icon-label.expand-component "Ausklappen")
  (icon-tooltip.expand-component "Unterformular ausklappen, um die Details zu sehen")

  (icon-label.collapse-component "Einklappen")
  (icon-tooltip.collapse-component "Unterformular einklappen und die Details verbergen")

  (icon-label.expand-from-reference "Ausdehnen")
  (icon-tooltip.expand-from-reference "Ausdehnen, um die Details zu sehen")

  (icon-label.collapse-to-reference "Zusammenklappen")
  (icon-tooltip.collapse-to-reference "Liste zusammenklappen")

  (icon-label.execute-filter "Filtern")
  (icon-tooltip.execute-filter "Daten nach den eingegebenen Kriterien filtern")

  (icon-label.find "Finden")
  (icon-tooltip.find "Ein Objekt finden")

  (icon-label.set-to-nil "Trennen")
  (icon-tooltip.set-to-nil "die Verbindung zu diesem Objekt trennen")

  (icon-label.set-to-unbound "Zurücksetzen")
  (icon-tooltip.set-to-unbound "den Wert dieses Feldes zurücksetzen")

  (icon-label.select-component "Auswählen")
  (icon-tooltip.select-component "Ein Objekt auswählen")

  (icon-label.view "Ansicht")
  (icon-tooltip.view "Ansicht wechseln")

  (icon-label.finish "Beenden")
  (icon-tooltip.finish "")

  (icon-label.cancel "Abbrechen")
  (icon-tooltip.cancel "die aktuelle Operation abbrechen, ohne etwas zu ändern")

  (icon-label.evaluate-form "Evaluieren")
  (icon-tooltip.evaluate-form "Ausdruck evaluieren und das Resultat zeigen")

  (icon-label.remove-list-element "Entfernen")
  (icon-tooltip.remove-list-element "Element von der Liste entfernen")

  (icon-label.hide-component "Verbergen")
  (icon-tooltip.hide-component "ein Objekt verbergen")

  (icon-label.external-link nil)
  (icon-tooltip.external-link "Externer Link")

  (icon-label.answer-component "Weiter")
  (icon-tooltip.answer-component "Weiter"))

;;;;;;
;;; Error handling

(def localization de
  (render-failed-to-load-page (&key &allow-other-keys)
    <div (:id ,+page-failed-to-load-id+)
      <h1 "Es dauert verdächtig lange, die Seite zu laden ...">
      <p "Manchmal kommen Internet-Browser beim Laden einer ansonsten gültigen Seite durcheinander. Sie können "
         <a (:href "#" :onclick "_wui_handleFailedToLoad()") "die Seite neu laden"> ", oder Sie verwenden die " <i "Neu laden"> "
         Schaltfläche Ihres Browsers, die das Problem auch beheben kann.">>))

;;;;;
;;; Context sensitive help

(def localization de
  (icon-label.context-sensitive-help "Hilfe-Text verfügbar")
  (context-sensitive-help.not-available "Hierzu ist kein Hilfe-Text verfügbar")
  (context-sensitive-help.self-description "This is the switch that can be used to turn on the context sensitive help. In help mode hovering the mouse over certain parts of the screen opens a tooltip just like this, but containing the most relevant help to that point (in this case the description of the help mode itself). A special mouse pointer indicates help mode. Clicking the mouse button anywhere in help mode turns it off."))

;;;;;;
;;; File up/download

(def localization de
  (file-last-modification-timestamp (file)
    `xml,"Updated: "
    (if (probe-file file)
        (localized-timestamp (local-time:universal-to-timestamp (file-write-date file)))
        <span (:class "missing-file")
              "File is missing!">))

  (icon-label.download "Download")
  (icon-tooltip.download "Download file")

  (icon-label.upload "Upload")
  (icon-tooltip.upload "Upload file"))

;;;;;;
;;; Interaction

(def localization de
  (interaction-aborted "Please correct the errors and try again"))

;;;;;;
;;; Chart

(def localization de
  (chart.missing-flash-plugin "Flash Plugin is not available"))

;;;;;;
;;; Diagram

(def localization de
  (icon-label.diagram "Diagram")
  (icon-tooltip.diagram "Show diagram"))

;;;;;;
;;; Cloneable

(def localization de
  (icon-label.open-in-new-frame "Neues Fenster")
  (icon-tooltip.open-in-new-frame "Objekt in einem neuen Fenster öffnen"))

;;;;;;
;;; Editing

(def localization de
  (icon-label.begin-editing "Bearbeiten")
  (icon-tooltip.begin-editing "Bearbeitung des angezeigten Objekts beginnen")

  (icon-label.save-editing "Speichern")
  (icon-tooltip.save-editing "Änderungen speichern und Bearbeitung beenden")

  (icon-label.cancel-editing "Abbrechen")
  (icon-tooltip.cancel-editing "Bearbeitung abbrechen, ohne zu speichern")

  (icon-label.store-editing "Zwischenspeichern")
  (icon-tooltip.store-editing "Bisherige Änderungen speichern, ohne die Bearbeitung zu beenden")

  (icon-label.revert-editing "Rückgängig")
  (icon-tooltip.revert-editing "Änderungen rückgängig machen"))

;;;;;;
;;; Export

(def localization de
  (icon-label.export-text "Text")
  (icon-tooltip.export-text "Export content in text format")

  (icon-label.export-csv "CSV")
  (icon-tooltip.export-csv "Export content in CSV format")

  (icon-label.export-pdf "PDF")
  (icon-tooltip.export-pdf "Export content in PDF format")

  (icon-label.export-odt "ODT")
  (icon-tooltip.export-odt "Export content in ODT format")

  (icon-label.export-ods "ODS")
  (icon-tooltip.export-ods "Export content in ODS format")

  (icon-label.export-ods "SH")
  (icon-tooltip.export-ods "Export content in shell script format"))

;;;;;;
;;; Refreshable

(def localization de
  (icon-label.refresh-component "Aktualisieren")
  (icon-tooltip.refresh-component "Inhalt aktualisieren"))

;;;;;;
;;; Filter

(def localization de
  (no-matches-were-found "Keine passenden Objekte gefunden")
  (matches-were-found (count) (format nil "~A passende(s) Objekt(e) gefunden" count))

  (icon-label.equal "gleich")
  (icon-tooltip.equal "Genau gleiche Daten suchen")

  (icon-label.like "ähnlich")
  (icon-tooltip.like "Ähnliche Daten suchen")

  (icon-label.< "kleiner")
  (icon-tooltip.< "Daten suchen, die kleiner sind")

  (icon-label.<= "kleiner gleich")
  (icon-tooltip.<= "Daten suchen die kleiner oder gleich sind")

  (icon-label.> "größer")
  (icon-tooltip.> "Daten suchen, die größer sind")

  (icon-label.>= "größer gleich")
  (icon-tooltip.>= "Daten suchen, die größer oder gleich sind")

  (icon-label.negated "Nicht")
  (icon-tooltip.negated "Daten suchen, die NICHT dem Kriterium entsprechen")

  (icon-label.ponated "Ponated")
  (icon-tooltip.ponated "Ponate condition")

  (predicate.equal "gleich")
  (predicate.like "ähnlich")
  (predicate.less-than "kleiner")
  (predicate.less-than-or-equal "kleiner gleich")
  (predicate.greater-than "größer")
  (predicate.greater-than-or-equal "größer gleich")
  (predicate.some "Some")
  (predicate.every "Alle"))

;;;;;;
;;; Movable

(def localization de
  (icon-label.move-backward "Move backward")
  (icon-tooltip.move-backward "Move object backward in the list"))

(def localization de
  (icon-label.move-forward "Move forward")
  (icon-tooltip.move-forward "Move object forward in the list"))

;;;;;;
;;; Primitive

(def localization de
  (value.default "default")
  (value.defaults-to "defaults to :")
  (value.unbound "default")
  (value.nil "none")

  (boolean.true "true")
  (boolean.false "false")

  (member-type-value.nil ""))

;;;;;;
;;; Authentication

(def localization de
  (login.title "Login")
  (login.identifier "Identifier")
  (login.password "Password")
  (login.message.authentication-failed "Authentication failed")
  (login.message.session-timed-out "Your session has timed out, please log in again")

  (icon-label.login "Login")
  (icon-tooltip.login "Gain privileges by authentication")

  (icon-label.logout "Logout")
  (icon-tooltip.logout "Leave the current session and remove all privileges previously gained by authentication"))

;;;;;;
;;; Expression

(def localization de
  (icon-label.add-expression-argument "Add")
  (icon-tooltip.add-expression-argument "Add new argument")

  (icon-label.remove-expression-argument "Remove")
  (icon-tooltip.remove-expression-argument "Remove argument"))

;;;;;;
;;; Menu

(def localization de
  (icon-label.show-submenu "Submenu")
  (icon-tooltip.show-submenu "Show submenu")

  (icon-label.show-context-menu "Menu")
  (icon-tooltip.show-context-menu "Show context menu")

  (context-menu.move-commands "Move"))

;;;;;;
;;; Page navigation

(def localization de
  (icon-label.go-to-first-page "First")
  (icon-tooltip.go-to-first-page "Jump to first page")

  (icon-label.go-to-previous-page "Previous")
  (icon-tooltip.go-to-previous-page "Go to previous page")

  (icon-label.go-to-next-page "Next")
  (icon-tooltip.go-to-next-page "go to next page")

  (icon-label.go-to-last-page "Last")
  (icon-tooltip.go-to-last-page "Jump to last page")

  (page-size-selector.rows/page " rows/page"))

;;;;;;
;;; Pivot table

(def localization de
  (icon-label.move-to-sheet-axes "Sheet axis")
  (icon-tooltip.move-to-sheet-axes "Move to sheet axes")

  (icon-label.move-to-row-axes "Row axis")
  (icon-tooltip.move-to-row-axes "Move to row axes")

  (icon-label.move-to-column-axes "Column axis")
  (icon-tooltip.move-to-column-axes "Move to column axes")

  (icon-label.move-to-cell-axes "Cell axis")
  (icon-tooltip.move-to-cell-axes "Move to cell axes")

  (class-name.pivot-table-axis-component "pivot table axis")

  (slot-name.sheet-axes "sheet axes")
  (slot-name.row-axes "row axes")
  (slot-name.column-axes "column axes")
  (slot-name.cell-axes "cell axes"))

;;;;;;
;;; Tab container

(def localization de
  (icon-label.switch-to-tab-page "Page")
  (icon-tooltip.switch-to-tab-page "Switch to page"))

;;;;;;
;;; Top

(def localization de
  (icon-label.focus-in "Focus")
  (icon-tooltip.focus-in "Focus on the object")

  (icon-label.focus-out "Back")
  (icon-tooltip.focus-out "Undo focus"))

;;;;;;
;;; Object

(def localization de
  (class-name.standard-object "standard object")

  (standard-object-detail-component.primary-group "Primary properties")
  (standard-object-detail-component.secondary-group "Other properties")

  (standard-object-slot-value-group.there-are-no-slots "There are no properties")

  (selectable-standard-object-tree-inspector.title (class-name)
    (string+ "Selecting an instance of " class-name))
  (standard-object-filter.title (class-name)
    (string+ "Searching for instances of " class-name))
  (standard-object-detail-filter.class-selector-label "Class")
  (standard-object-detail-filter.ordering-specifier-label "Ordering")
  (standard-object-list-inspector.title (class-name)
    (string+ "Viewing instances of " class-name))
  (standard-object-tree-inspector.title (class-name)
    (string+ "Viewing a tree of " class-name))
  (standard-object-inspector.title (class-name)
    (string+ "Viewing an instance of " class-name))
  (standard-object-maker.title (class-name)
    (string+ "Creating an instance of " class-name))

  (object-list-table.column.commands "")
  (object-list-table.column.type "Type")
  (object-tree-table.column.commands "")
  (object-tree-table.column.type "Type")
  (standard-object-detail-maker.class-selector-label "Class")

  (delete-instance.dialog.title "Deleting an object")
  (delete-instance.dialog.body (&key instance &allow-other-keys)
    <p "Are you sure you wan to delete the following object?"
       <br>
       ;; TODO replace with a reference renderer
       ,(princ-to-string instance)>))
