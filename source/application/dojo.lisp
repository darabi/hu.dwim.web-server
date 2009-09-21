;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Dojo support

(def special-variable *dojo-widget-ids*)

(macrolet ((x (&body entries)
             `(progn
                ,@(iter (for (name dojo-name) :on entries :by #'cddr)
                        (collect `(def (constant e :test 'string=) ,name ,dojo-name))))))
  (x
   +dijit/button+                "dijit.form.Button"
   +dijit/drop-down-button+      "dijit.form.DropDownButton"
   +dijit/date-text-box+         "dijit.form.DateTextBox"
   +dijit/simple-text-area+      "dijit.form.SimpleTextarea"
   +dijit/text-box+              "dijit.form.TextBox"
   +dijit/time-text-box+         "dijit.form.TimeTextBox"
   +dijit/combo-box+             "dijit.form.ComboBox"
   +dijit/filtering-select+      "dijit.form.FilteringSelect"
   +dijit/number-text-box+       "dijit.form.NumberTextBox"
   +dijit/dialog+                "dijit.Dialog"
   +dijit/tooltip-dialog+        "dijit.TooltipDialog"
   +dijit/editor+                "dijit.Editor"
   +dijit/menu+                  "dijit.Menu"
   +dijit/menu-bar+              "dijit.MenuBar"
   +dijit/menu-item+             "dijit.MenuItem"
   +dijit/menu-bar-item+         "dijit.MenuBarItem"
   +dijit/menu-separator+        "dijit.MenuSeparator"
   +dijit/popup-menu-item+       "dijit.PopupMenuItem"
   +dijit/popup-menu-bar-item+   "dijit.PopupMenuBarItem"
   +dijit/inline-edit-box+       "dijit.InlineEditBox"
   ))

(def (function e) find-latest-dojo-directory-name (www-directory)
  (loop
     (with-simple-restart (retry "Try searching for dojo directories again in ~A" www-directory)
       (bind ((dojo-dir (first (sort (remove-if [not (starts-with-subseq "dojo" !1)]
                                                (mapcar [last-elt (pathname-directory !1)]
                                                        (cl-fad:list-directory www-directory)))
                                     #'string>=))))
         (if dojo-dir
             (return (string+ dojo-dir "/"))
             (cerror "Ignore" "Seems like there's not any dojo directory in ~S. Hint: see hu.dwim.wui/etc/build-dojo.sh" www-directory))))))

(def with-macro with-dojo-widget-collector ()
  (bind ((*dojo-widget-ids* nil))
    (multiple-value-prog1
        (-body-)
      (when *dojo-widget-ids*
        ;; NOTE: this must come first before any additional widget setup
        `xml,@(with-collapsed-js-scripts
               `js(on-load
                   (let ((widget-ids (array ,@*dojo-widget-ids*)))
                     (log.debug "Instantiating (and destroying previous versions of) the following widgets " widget-ids)
                     (dolist (widget-id widget-ids)
                       (awhen (dijit.byId widget-id)
                         (.destroyRecursive it)))
                     (dojo.parser.instantiate (map 'dojo.by-id widget-ids)))))))))

(def macro render-dojo-widget ((&optional (id '(generate-frame-unique-string "_w")))
                                &body body)
  (once-only (id)
    `(progn
       ,@body
       (push ,id *dojo-widget-ids*)
       (values))))

(def (macro e) render-dojo-dialog ((id-var &key title (width "400px") (attach-point '`js-piece(slot-value document.body ,(generate-response-unique-string))))
                                    &body body)
  (once-only (width attach-point)
    `(bind ((,id-var (generate-frame-unique-string)))
       `js(bind ((dialog (or ,,attach-point
                             (new dijit.Dialog (create :id ,,id-var
                                                       :title ,,title
                                                       :content ,(transform-xml/js-quoted ,@body))))))
            (setf ,,attach-point dialog)
            ;; TODO qq should work with ,,(when ...)
            (bind ((width ,,width))
              (when width
                (dojo.style dialog.domNode "width" width)))
            (dialog.show)))))

(def (macro e) render-dojo-dialog/buttons (&body entires)
  `<table (:style "float: right;")
     <tr ,@,@(iter (for (label js) :in entires)
                   (collect `<td ,(render-dojo-button ,label :on-click ,js)>))>>)

(def (macro e) render-dojo-button (label &key on-click)
  `<div (:dojoType   #.+dijit/button+
         :label      ,,label
         :onClick    ,,on-click)>)

#+nil ;; TODO this should work instead of the above
(def (macro e) render-dojo-button (label &key on-click)
  `(render-dojo-widget* (+dijit/button+ :label ,label :on-click ,on-click)))

(def (macro e) render-dojo-widget* ((dojo-type &rest attributes &key id (xml-element-name "span")
                                               &allow-other-keys)
                                     &body body)
  (remove-from-plistf attributes :id :xml-element-name)
  `<,,xml-element-name (:id ,,id
                        :dojoType ,,dojo-type
                        ,@,@(iter (for (name value) :on attributes :by #'cddr)
                                  (collect (make-xml-attribute (hyphened-to-camel-case (string-downcase name)) (make-xml-unquote value)))))
                       ,@,@body>)
