;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;; Error handling
(def resources en
  (render-failed-to-load-page (&key &allow-other-keys)
    <div (:id ,+page-failed-to-load-id+)
      <h1 "It takes suspiciously long to load the page...">
      <p "Unfortunately sometimes certain browsers get confused even when loading an otherwise valid page. You can try to "
         <a (:href "#" :onclick "_wui_handleFailedToLoad()") "reload the page"> ", or use the " <i "Refresh"> "
         button of your browser, which usually solves the problem.">>))

;;; Context sensitive help
(def resources en
  (icon-label.help "Help")
  (help.no-context-sensitive-help-available "No conext sensitive help available")
  (help.help-about-context-sensitive-help-button "This is the switch that can be used to turn on the context sensitive help. In help mode hovering the mouse over certain parts of the screen opens a tooltip just like this, but containing the most relevant help to that point (in this case the description of the help mode itself). A special mouse pointer indicates help mode. Clicking the mouse button anywhere in help mode turns off the mode."))

;;; MetaGUI
(def resources en
  (selectable-standard-object-tree-inspector.title (class-name)
    `xml,"Selecting an instance of " `xml,class-name)
  (standard-object-filter.title (class-name)
    `xml,"Searching for instances of " `xml,class-name)
  (standard-object-detail-filter.class-selector-label "Class")
  (standard-object-detail-filter.ordering-specifier-label "Ordering")
  (standard-object-list-inspector.title (class-name)
    `xml,"Viewing instances of " `xml,class-name)
  (standard-object-tree-inspector.title (class-name)
    `xml,"Viewing a tree of " `xml,class-name)
  (standard-object-inspector.title (class-name)
    `xml,"Viewing an instance of " `xml,class-name)
  (standard-object-maker.title (class-name)
    `xml,"Creating an instance of " `xml,class-name)

  (object-list-table.column.commands "")
  (object-list-table.column.type "Type")
  (object-tree-table.column.commands "")
  (object-tree-table.column.type "Type")
  (standard-object-detail-maker.class-selector-label "Class")

  (delete-instance.dialog.title "Deleting an object")
  (delete-instance.dialog.body (&key class instance &allow-other-keys)
    (declare (ignore class))
    <p "Are you sure you wan to delete the object "
       ;; TODO replace with a reference renderer
       ,(princ-to-string instance)
       " ?">)
  )

;;; File up/download
(def resources en
  (file-last-modification-timestamp (file)
    `xml,"Updated: "
    (if (probe-file file)
        (localized-timestamp (local-time:universal-to-timestamp (file-write-date file)))
        <span (:class "missing-file")
              "File is missing!">)))