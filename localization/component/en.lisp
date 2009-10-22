;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icon

(def localization en
  (icon-label.new "New")
  (icon-tooltip.new "Start editing a new object")

  (icon-label.create "Create")
  (icon-tooltip.create "Create object")

  (icon-label.delete "Delete")
  (icon-tooltip.delete "Delete object")

  (icon-label.close "Close")
  (icon-tooltip.close "Close the component")

  (icon-label.back "Back")
  (icon-tooltip.back "Move back")

  (icon-label.expand-component "Expand")
  (icon-tooltip.expand-component "Show details")

  (icon-label.collapse-component "Collapse")
  (icon-tooltip.collapse-component "Hide details")

  (icon-label.expand-from-reference "Expand")
  (icon-tooltip.expand-from-reference "Expand to detail")

  (icon-label.collapse-to-reference "Collapse")
  (icon-tooltip.collapse-to-reference "Collapse to reference")

  (icon-label.execute-filter "Filter")
  (icon-tooltip.execute-filter "Execute the filter")

  (icon-label.find "Find")
  (icon-tooltip.find "Find an object")

  (icon-label.set-to-nil "Disconnect")
  (icon-tooltip.set-to-nil "Disconnect from object")

  (icon-label.set-to-unbound "Default")
  (icon-tooltip.set-to-unbound "Set to default")

  (icon-label.select-component "Select")
  (icon-tooltip.select-component "Select an object")

  (icon-label.view "View")
  (icon-tooltip.view "Change view")

  (icon-label.finish "Finish")
  (icon-tooltip.finish "Finish wizard")

  (icon-label.cancel "Cancel")
  (icon-tooltip.cancel "Cancel the current operation without modifying anything")

  (icon-label.evaluate-form "Evaluate")
  (icon-tooltip.evaluate-form "Evaluate form and display result")

  (icon-label.remove-list-element "Remove")
  (icon-tooltip.remove-list-element "Remove element from list")

  (icon-label.hide-component "Hide")
  (icon-tooltip.hide-component "Hide object"))

;;;;;;
;;; Error handling

(def localization en
  (render-failed-to-load-page (&key &allow-other-keys)
    <div (:id ,+page-failed-to-load-id+)
      <h1 "It takes suspiciously long to load the page...">
      <p "Unfortunately sometimes certain browsers get confused even when loading an otherwise valid page. You can try to "
         <a (:href "#" :onclick "_wui_handleFailedToLoad()") "reload the page"> ", or use the " <i "Refresh"> "
         button of your browser, which usually solves the problem.">>))

;;;;;
;;; Context sensitive help

(def localization en
  (icon-label.help "Help")
  (help.no-context-sensitive-help-available "No conext sensitive help available")
  (help.help-about-context-sensitive-help-button "This is the switch that can be used to turn on the context sensitive help. In help mode hovering the mouse over certain parts of the screen opens a tooltip just like this, but containing the most relevant help to that point (in this case the description of the help mode itself). A special mouse pointer indicates help mode. Clicking the mouse button anywhere in help mode turns off the mode."))

;;;;;;
;;; File up/download

(def localization en
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

(def localization en
  (interaction-aborted "Please correct the errors and try again"))

;;;;;;
;;; Chart

(def localization en
  (chart.missing-flash-plugin "Flash Plugin is not available"))

;;;;;;
;;; Diagram

(def localization en
  (icon-label.diagram "Diagram")
  (icon-tooltip.diagram "Show diagram"))

;;;;;;
;;; Cloneable

(def localization en
  (icon-label.open-in-new-frame "New window")
  (icon-tooltip.open-in-new-frame "Open object in new window"))

;;;;;;
;;; Editing

(def localization en
  (icon-label.begin-editing "Edit")
  (icon-tooltip.begin-editing "Start editing")

  (icon-label.save-editing "Save")
  (icon-tooltip.save-editing "Save changes and finish editing")

  (icon-label.cancel-editing "Cancel")
  (icon-tooltip.cancel-editing "Cancel changes and finish editing")

  (icon-label.store-editing "Store")
  (icon-tooltip.store-editing "Store changes")

  (icon-label.revert-editing "Revert")
  (icon-tooltip.revert-editing "Revert changes"))

;;;;;;
;;; Export

(def localization en
  (icon-label.export-text "Text")
  (icon-tooltip.export-text "Export content in text format")

  (icon-label.export-csv "CSV")
  (icon-tooltip.export-csv "Export content in CSV format")

  (icon-label.export-pdf "PDF")
  (icon-tooltip.export-pdf "Export content in PDF format")

  (icon-label.export-odt "ODT")
  (icon-tooltip.export-odt "Export content in ODT format")

  (icon-label.export-ods "ODS")
  (icon-tooltip.export-ods "Export content in ODS format"))

;;;;;;
;;; Refreshable

(def localization en
  (icon-label.refresh-component "Refresh")
  (icon-tooltip.refresh-component "Refresh content"))

;;;;;;
;;; Filter

(def localization en
  (no-matches-were-found "No matching objects were found")
  (matches-were-found (count) (format nil "~A matching objects were found" count))

  (icon-label.equal "Equal")
  (icon-tooltip.equal "Compare for equality")

  (icon-label.like "Like")
  (icon-tooltip.like "Compare for like")

  (icon-label.< "Less")
  (icon-tooltip.< "Compare for less then")

  (icon-label.<= "Less or equal")
  (icon-tooltip.<= "Compare for less than or equal")

  (icon-label.> "Greater")
  (icon-tooltip.> "Compare for greater then")

  (icon-label.>= "Greater or equal")
  (icon-tooltip.>= "Compare for greater than or equal")

  (icon-label.negated "Negated")
  (icon-tooltip.negated "Negate condition")

  (icon-label.ponated "Ponated")
  (icon-tooltip.ponated "Ponate condition")

  (predicate.equal "Equal")
  (predicate.like "Like")
  (predicate.less-than "Less than")
  (predicate.less-than-or-equal "Less than or equal")
  (predicate.greater-than "Greater than")
  (predicate.greater-than-or-equal "Greater than or equal"))

;;;;;;
;;; Movable

(def localization en
  (icon-label.move-backward "Move backward")
  (icon-tooltip.move-backward "Move object backward in the list"))

(def localization en
  (icon-label.move-forward "Move forward")
  (icon-tooltip.move-forward "Move object forward in the list"))

;;;;;;
;;; Primitive

(def localization en
  (value.default "default")
  (value.defaults-to "defaults to :")
  (value.unbound "default")
  (value.nil "none")

  (boolean.true "true")
  (boolean.false "false")

  (member-type-value.nil ""))

;;;;;;
;;; Authentication

(def localization en
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

(def localization en
  (icon-label.add-expression-argument "Add")
  (icon-tooltip.add-expression-argument "Add new argument")

  (icon-label.remove-expression-argument "Remove")
  (icon-tooltip.remove-expression-argument "Remove argument"))

;;;;;;
;;; Menu

(def localization en
  (icon-label.show-submenu "Submenu")
  (icon-tooltip.show-submenu "Show submenu")

  (icon-label.show-context-menu "Menu")
  (icon-tooltip.show-context-menu "Show context menu")

  (context-menu.move-commands "Move"))

;;;;;;
;;; Page navigation

(def localization en
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

(def localization en
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

(def localization en
  (icon-label.switch-to-tab-page "Page")
  (icon-tooltip.switch-to-tab-page "Switch to page"))

;;;;;;
;;; Top

(def localization en
  (icon-label.focus-in "Focus")
  (icon-tooltip.focus-in "Focus on the object")

  (icon-label.focus-out "Back")
  (icon-tooltip.focus-out "Undo focus"))

;;;;;;
;;; Object

(def localization en
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

;;;;;;
;;; Class

(def localization en
  (class-name.structure-class "structure class")
  (class-name.standard-class "standard class")
  (class-name.component-class "component class")
  (class-name.function "function")
  (class-name.standard-generic-function "standard generic function")

  (slot-name.documentation "documentation")
  (slot-name.%documentation "documentation"))

;;;;;;
;;; Component

(def localization en
  (class-name.component "component"))

;;;;;;
;;; Dictionary

(def localization en
  (dictionary-name.editing "Editing"))
