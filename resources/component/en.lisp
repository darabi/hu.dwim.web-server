;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;; Error handling
(def resources en
  (render-failed-to-load-page (&key &allow-other-keys)
    <div (:id ,+page-failed-to-load-id+)
      <h1 "It takes suspiciously long to load the page...">
      <p "Unfortunately certain browsers sometimes get confused when loading the page. Try to "
         <a (:href "#" :onclick "_wui_handleFailedToLoad()") "reload the page">
         ", and if it doesn't help, then clear the browser cache.">>))

;;; Context sensitive help
(def resources en
  (icon-label.help "Help")
  (help.no-context-sensitive-help-available "No conext sensitive help available")
  (help.help-about-context-sensitive-help-button "This is the switch that can be used to turn on the context sensitive help. In help mode hovering the mouse over certain parts of the screen opens a tooltip just like this, but containing the most relevant help to that point (in this case the description of the help mode itself). A special mouse pointer indicates help mode. Clicking the mouse button anywhere in help mode turns off the mode."))

;;; MetaGUI
(def resources en
  (selectable-standard-object-tree-table-inspector.title (class)
    `xml,"Selecting an instance of " (render class))
  (standard-object-detail-filter.title (class)
    `xml,"Searching for instances of" (render class))
  (standard-object-detail-filter.class-selector-label "Class")
  (standard-object-detail-filter.ordering-specifier-label "Ordering")
  (standard-object-list-table-inspector.title (class)
    `xml,"Viewing instances of " (render class))
  (standard-object-tree-table-inspector.title (class)
    `xml,"Viewing a tree of " (render class))
  (standard-object-detail-inspector.title (class)
    `xml,"Viewing an instance of " (render class))
  (standard-object-detail-maker.title (class)
    `xml,"Creating an instance of" (render class))

  (object-list-table.column.commands "")
  (object-list-table.column.type "Type")
  (object-tree-table.column.commands "")
  (object-tree-table.column.type "Type")
  (standard-object-detail-maker.class-selector-label "Class")
  )

