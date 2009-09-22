;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui.test)

;;;;;;
;;; This is a simple example application with various components

(setf *dojo-directory-name* (find-latest-dojo-directory-name (system-relative-pathname :hu.dwim.wui.test "www/")))

(def (constant :test #'equal) +demo-stylesheet-uris+ (append (flet ((entry (path)
                                                                      (list (string+ "static/" path)
                                                                            (assert-file-exists (system-relative-pathname :hu.dwim.wui.test (string+ "www/" path)))))
                                                                    (dojo-relative-path (path)
                                                                      (string+ *dojo-directory-name* path)))
                                                               (list (entry "wui/css/wui.css")
                                                                     (entry "wui/css/icon.css")
                                                                     (entry "wui/css/widget.css")
                                                                     (entry (dojo-relative-path "dijit/themes/tundra/tundra.css"))
                                                                     (entry (dojo-relative-path "dojo/resources/dojo.css"))))))

(def (constant :test #'equal) +demo-script-uris+ '("wui/js/wui.js" "wui/js/component-hierarchy.js"))

(def (constant :test #'equal) +demo-page-icon+ "static/favicon.ico")

(def class* demo-application (application-with-home-package
                              application-with-dojo-support)
  ()
  (:metaclass funcallable-standard-class))

(def class* demo-session ()
  ((authenticated-subject nil)
   (example-inline-edit-box-value "initial value of the example inline edit box")))

(def function current-authenticated-subject ()
  (and *session*
       (authenticated-subject-of *session*)))

(def function is-authenticated? ()
  (not (null (current-authenticated-subject))))

(def method session-class list ((application demo-application))
  'demo-session)

(def special-variable *demo-application*
  (make-instance 'demo-application
                 :path-prefix "/"
                 :home-package (find-package :hu.dwim.wui.test)
                 :default-locale "en"
                 :ajax-enabled #t))

(def layered-method make-frame-component-with-content ((application demo-application) session frame content)
  (make-demo-frame-component-with-content content))

;;;;;;
;;; Entry point

(def file-serving-entry-point *demo-application* "/static/" (system-relative-pathname :hu.dwim.wui "www/"))

(def file-serving-entry-point *demo-application* "/darcs/" *workspace-directory*)

(def file-serving-entry-point *demo-application* "/darcsweb/" (merge-pathnames "darcsweb/" *workspace-directory*))

(def js-file-serving-entry-point *demo-application* "/wui/js/" (system-relative-pathname :hu.dwim.wui "source/js/"))

(def js-component-hierarchy-serving-entry-point *demo-application* "wui/js/component-hierarchy.js")

(def entry-point (*demo-application* :path "darcsweb" :with-session-logic #f) ()
  (make-raw-functional-response ()
    (handle-cgi-request (merge-pathnames "darcsweb/darcsweb.cgi" *workspace-directory*))))

(def entry-point (*demo-application* :path "" :ensure-session #t :ensure-frame #t) ()
  (assert (and (boundp '*session*) *session*))
  (assert (and (boundp '*frame*) *frame*))
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*) (make-demo-frame-component))
        (make-redirect-response-for-current-application))))

(def function start-test-server-with-demo-application (&key (maximum-worker-count 16) (log-level +debug+) (host *test-host*) (port *test-port*))
  (setf (log-level 'wui) log-level)
  (start-test-server-with-brokers (list *demo-application*
                                        (make-redirect-broker "" "/"))
                                  :host host
                                  :port port
                                  :maximum-worker-count maximum-worker-count
                                  :request-content-length-limit (* 1024 1024 50)))

;;;;;;
;;; Component demo

(def macro component-demo/widget (content &body forms)
  `(node/widget ()
       (replace-target-place/widget ()
           ,content
         (lisp-form/component-demo/inspector ()
           ,@forms))))

(def function make-demo-frame-component ()
  (make-demo-frame-component-with-content))

(def function make-demo-frame-component-with-content (&optional initial-content-component)
  (frame/widget (:title "demo"
                 :stylesheet-uris +demo-stylesheet-uris+
                 :script-uris +demo-script-uris+
                 :page-icon +demo-page-icon+)
    (top/widget (:menu-bar (menu-bar/widget ()
                             (make-debug-menu)))
      (make-demo-content initial-content-component))))

;;;;;;
;;; Immediate

(def function make-immediates-node ()
  (node/widget (:expanded #f)
      "Immediate"
    (component-demo/widget "Number"
      42)
    (component-demo/widget "String"
      "John")))

;;;;;
;;; Layout

(def function make-layouts-node ()
  (node/widget (:expanded #f)
      "Layout"
    (component-demo/widget "Empty"
      (empty/layout))
    (component-demo/widget "Alternator"
      ;; NOTE: a layout does not have behavior, so switching between alternative is only possible programatically
      (alternator/layout ()
        "John"
        "Mary"
        "Steve"
        "Kate"))
    (component-demo/widget "Vertical List"
      (vertical-list/layout ()
        "John"
        "Mary"
        "Steve"
        "Kate"))
    (component-demo/widget "Horizontal List"
      (horizontal-list/layout ()
        "John"
        "Mary"
        "Steve"
        "Kate"))
    (component-demo/widget "Flow"
      (flow/layout ()
        "John "
        "Mary "
        "Steve "
        "Kate "
        "Fred "
        "Susanne "
        "George "
        "Jenna "))
    (component-demo/widget "Container"
      ;; NOTE: see #container in style.css
      (container/layout (:id "container")
        "John "
        "Mary "
        "Steve "
        "Kate "
        "Fred "))
    (component-demo/widget "Table"
      (table/layout ()
        (row/layout ()
          (cell/layout ()
            "John")
          (cell/layout ()
            "Mary"))
        (row/layout ()
          (cell/layout ()
            "Steve")
          (cell/layout ()
            "Kate"))))
    (component-demo/widget "Tree"
      (tree/layout ()
        (node/layout ()
            "Males"
          (node/layout ()
              "John")
          (node/layout ()
              "Steve"))
        (node/layout ()
            "Females"
          (node/layout ()
              "Mary")
          (node/layout ()
              "Kate"))))
    (component-demo/widget "Treeble"
      (treeble/layout ()
        (nodrow/layout (:cells (list (cell/layout ()
                                       "John")
                                     (cell/layout ()
                                       "Mary")))
          (nodrow/layout (:cells (list (cell/layout ()
                                         "Steve")
                                       (cell/layout ()
                                         "Kate")))
            (nodrow/layout (:cells (list (cell/layout ()
                                         "George")
                                       (cell/layout ()
                                         "Susanne"))))))))
    (component-demo/widget "XY"
      (xy/layout (:width 200 :height 200)
        (parent-relative-position/layout (:x 100 :y 100)
          "John")
        (parent-relative-position/layout (:x 50 :y 150)
          "Mary")
        (parent-relative-position/layout (:x 120 :y 70)
          "Steve")
        (parent-relative-position/layout (:x 80 :y 50)
          "Kate")))))

;;;;;;
;;; Widget

(def function make-widgets-node ()
  (node/widget (:expanded #f)
      "Widget"
    (component-demo/widget "Inline render XHTML"
      (inline-render-xhtml/widget ()
        <div <span (:style "color: blue") "John">
             <span (:style "color: red") "Mary">>))
    (component-demo/widget "Wrap render XHTML"
      (wrap-render-xhtml/widget ()
          "The wrapped component is now a simple string"
        <span ">>> "
              <span (:style "color: blue")
                    ,(-body-)>
              " <<<">))
    (component-demo/widget "Inline XHTML string content"
      (inline-xhtml-string-content/widget ()
        "<div><span style=\"color: blue\">John</span><span style=\"color: red\">Mary</span></div>"))
    (component-demo/widget "Quote XML string content"
      (quote-xml-string-content/widget ()
        "<div><span style=\"color: blue\">John</span><span style=\"color: red\">Mary</span></div>"))
    (component-demo/widget "Quote XML form"
      (quote-xml-form/widget ()
        <div <span (:style ,(string+ "color:" " blue")) "John">
             <span (:style "color: red") "Mary">>))
    (component-demo/widget "Collapsible"
      (collapsible/widget ()
        "SICP"
        "Structure and Interpretation of Computer Programs"))
    (component-demo/widget "Alternator"
      (alternator/widget ()
        "John"
        "Mary"
        "Steve"
        "Kate"))
    (component-demo/widget "Tab container"
      (tab-container ()
        (tab-page (:selector (icon switch-to-tab-page :label "Male"))
          "John")
        (tab-page (:selector (icon switch-to-tab-page :label "Female"))
          "Mary")))
    (component-demo/widget "Menu bar"
      (menu-bar/widget ()
        (menu-item/widget ()
            "John"
          (menu-item/widget ()
              "Mary")
          (menu-item-separator/widget)
          (menu-item/widget ()
              "Steve"
            (menu-item/widget ()
                "Kate")
            (menu-item/widget ()
                "Fred")))
        (menu-item/widget ()
            "Susanne "
          (menu-item/widget ()
              "George ")
          (menu-item/widget ()
              "Jenna "))))
    (component-demo/widget "Popup menu"
      (popup-menu/widget ()
          "Right click for popup menu"
        (menu-item/widget ()
            "John"
          (menu-item/widget ()
              "Mary")
          (menu-item-separator/widget)
          (menu-item/widget ()
              "Steve"
            (menu-item/widget ()
                "Kate")
            (menu-item/widget ()
                "Fred")))
        (menu-item/widget ()
            "Susanne "
          (menu-item/widget ()
              "George ")
          (menu-item/widget ()
              "Jenna "))))
    (component-demo/widget "Context menu"
      (content/widget (:context-menu (context-menu/widget ()
                                       (menu-item/widget ()
                                           "John"
                                         (menu-item/widget ()
                                             "Mary")
                                         (menu-item-separator/widget)
                                         (menu-item/widget ()
                                             "Steve"
                                           (menu-item/widget ()
                                               "Kate")
                                           (menu-item/widget ()
                                               "Fred")))
                                       (menu-item/widget ()
                                           "Susanne "
                                         (menu-item/widget ()
                                             "George ")
                                         (menu-item/widget ()
                                             "Jenna "))))
        "Right click for context menu"))
    (component-demo/widget "Command"
      (bind ((counter 0)
             (component nil))
        (setf component
              (contents/widget ()
                (vertical-list/layout ()
                  (command/widget ()
                    "Increment"
                    (make-action
                      (incf counter)))
                  (command/widget (:ajax #t)
                    "Increment (ajax)"
                    (make-action
                      (incf counter)
                      (mark-to-be-rendered-component component))))
                (inline-render-xhtml/widget ()
                  <div "Counter: " ,counter>)))))
    (component-demo/widget "Command bar"
      (bind ((string nil))
        (vertical-list/layout ()
          (command-bar/widget ()
            (command/widget ()
              (icon refresh-component)
              (make-action
                (setf string "refresh")))
            (command/widget ()
              (icon select-component)
              (make-action
                (setf string "select"))))
          (inline-render-xhtml/widget ()
            <span "Last command: " ,string>))))
    (component-demo/widget "Push button"
      (push-button/widget ()
        (command ()
          (icon refresh-component)
          (make-action))))
    (component-demo/widget "Toggle button"
      (toggle-button/widget ()
        (command ()
          (icon refresh-component)
          (make-action))))
    (component-demo/widget "Drop down button"
      (drop-down-button/widget ()
        (command ()
          (icon refresh-component)
          (make-action))))
    (component-demo/widget "List"
      (list/widget ()
        (element/widget ()
          "John")
        (element/widget ()
          "Mary")
        (element/widget ()
          "Steve")
        (element/widget ()
          "Kate")))
    (component-demo/widget "Name value list"
      (name-value-list/widget ()
        (name-value-group/widget (:title "Name")
          (name-value-pair/widget ()
            "First Name"
            "John")
          (name-value-pair/widget ()
            "Last Name"
            "Doe"))
        (name-value-group/widget (:title "Other")
          (name-value-pair/widget ()
            "Sex"
            "Male")
          (name-value-pair/widget ()
            "Age"
            "34"))))
    (component-demo/widget "Panel"
      (panel/widget (:title-bar (title-bar/widget ()
                                  "The panel's title")
                     :command-bar (command-bar/widget ()
                                    (command/widget ()
                                      (icon refresh-component)
                                      (make-action))))
        "John"))
    (component-demo/widget "Information message"
      (component-message/widget (:category :information)
        "John has been added to the list of males"))
    (component-demo/widget "Warning message"
      (component-message/widget (:category :warning)
        "John has been alreay added to the list of males"))
    (component-demo/widget "Error message"
      (component-message/widget (:category :error)
        "Cannot add John to the list of females, he is a male"))
    (component-demo/widget "Component messages"
      (component-messages/widget ()
        (component-message/widget (:permanent #t :category :information)
          "John has been added to the list of males")
        (component-message/widget (:permanent #t :category :warning)
          "John has been alreay added to the list of males")
        (component-message/widget (:permanent #t :category :error)
          "Cannot add John to the list of females, he is a male")
        (component-message/widget (:permanent #t :category :information)
          "Mary has been added to the list of females")
        (component-message/widget (:permanent #t :category :warning)
          "Mary has been alreay added to the list of females")
        (component-message/widget (:permanent #t :category :error)
          "Cannot add Mary to the list of males, she is a female")))
    (component-demo/widget "Splitter"
      (splitter/widget ()
        "TODO"))
    (component-demo/widget "Table"
      (table/widget (:columns (list (column/widget ()
                                      "Male")
                                    (column/widget ()
                                      "Female")))
        (row/widget (:header "1")
          (cell/widget ()
            "John")
          (cell/widget ()
            "Mary"))
        (entire-row/widget (:header "2")
          "Entire row")
        (row/widget (:header "3")
          (cell/widget ()
            "Steve")
          (cell/widget ()
            "Kate"))))
    (component-demo/widget "Tree"
      (tree/widget ()
        (node/widget ()
            "John"
          (node/widget ()
              "Mary")
          (node/widget ()
              "Steve")
          (node/widget ()
              "Kate"
            (node/widget ()
                "Fred")
            (node/widget ()
                "Susanne")))))
    (component-demo/widget "Treeble"
      (treeble/widget (:columns (list (column/widget ()
                                        "Male")
                                      (column/widget ()
                                        "Female")))
        (nodrow/widget (:cells (list (cell/widget ()
                                       "John")
                                     (cell/widget ()
                                       "Mary")))
          (nodrow/widget (:cells (list (cell/widget ()
                                         "Steve")
                                       (cell/widget ()
                                         "Kate")))
            (nodrow/widget (:cells (list (cell/widget ()
                                           "George")
                                         (cell/widget ()
                                           "Susanne"))))))))
    (component-demo/widget "Tree navigator"
      (make-instance 'tree-level/widget
                     :path (path/widget () "Magyarország" "Dél-dunántúli régió")
                     :previous-sibling "Észak-magyarországi régió"
                     :next-sibling "Közép-magyarországi régió"
                     :descendants (tree/widget ()
                                    (node/widget ()
                                        "Pest megye"
                                      (node/widget ()
                                          "Budapest")
                                      (node/widget ()
                                          "Érd"))
                                    (node/widget ()
                                        "Zala megye"
                                      (node/widget ()
                                          "Zala"))
                                    (node/widget ()
                                        "Fejér megye"
                                      (node/widget ()
                                          "Székesfehérvár")
                                      (node/widget ()
                                          "Agárd")))
                     :node "Dél-magyarországi régió"))
    (component-demo/widget "Graph"
      (graph/widget ()
        (vertex/widget (:vertex-id 0)
          "John")
        (vertex/widget (:vertex-id 1)
          "Mary")
        (vertex/widget (:vertex-id 2)
          "Steve")
        (vertex/widget (:vertex-id 3)
          "Kate")
        (edge/widget (:vertex-1 0 :vertex-2 1)
          "Likes")
        (edge/widget (:vertex-1 1 :vertex-2 2)
          "Dislikes")
        (edge/widget (:vertex-1 2 :vertex-2 3)
          "Friendly")
        (edge/widget (:vertex-1 3 :vertex-2 0)
          "Unfriendly")))))

;;;;;;
;;; Primitive

(def function make-primitive-makers-node ()
  (make-primitive-presentations-node "Maker" 'make-maker))

(def function make-primitive-viewers-node ()
  (make-primitive-presentations-node "Viewer" 'make-viewer))

(def function make-primitive-editors-node ()
  (make-primitive-presentations-node "Editor" 'make-editor))

(def function make-primitive-inspectors-node ()
  (make-primitive-presentations-node "Inspector" 'make-inspector))

(def function make-primitive-filters-node ()
  (make-primitive-presentations-node "Filter" 'make-filter))

(def function make-primitive-presentations-node (name factory)
  (flet ((make (type value &rest args)
           (apply factory type value :initial-alternative-type 't/detail/presentation args)))
    (node/widget (:expanded #f)
        name
      (component-demo/widget "Boolean"
        (make 'boolean #f))
      (component-demo/widget "Character"
        (make 'character  #\J))
      (component-demo/widget "String"
        (make 'string  "John"))
      (component-demo/widget "Password"
        (make 'hu.dwim.wui::password "Mary"))
      (component-demo/widget "Symbol"
        (make 'symbol 'John))
      (component-demo/widget "Number"
        (make 'number 12345678901234567890/10000000000))
      (component-demo/widget "Integer"
        (make 'integer 42))
      (component-demo/widget "Float"
        (make 'float 42.42))
      (component-demo/widget "Date"
        (make 'date (local-time:now)))
      (component-demo/widget "Time"
        (make 'time (local-time:now)))
      (component-demo/widget "Timestamp"
        (make 'timestamp (local-time:now)))
      (component-demo/widget "Member"
        (make 'member 'John :possible-values '(John Mary Steve Kate)))
      (component-demo/widget "HTML"
        (make 'html "John <b>Mary</b> <h1>Steve</h1> <i>Kate</i>"))
      (component-demo/widget "IP address"
        ;; TODO: component-value
        (make 'ip-address nil))
      (component-demo/widget "File"
        ;; TODO: component-value
        (make 'file nil)))))

;;;;;;
;;; Place

(def function make-place-makers-node ()
  (make-place-presentations-node "Maker" 'make-place-maker))

(def function make-place-viewers-node ()
  (make-place-presentations-node "Viewer" 'make-place-viewer))

(def function make-place-editors-node ()
  (make-place-presentations-node "Editor" 'make-place-editor))

(def function make-place-inspectors-node ()
  (make-place-presentations-node "Inspector" 'make-place-inspector))

(def function make-place-filters-node ()
  (make-place-presentations-node "Filter" 'make-place-filter))

(def special-variable *person-name* "John")

(def function make-place-presentations-node (name factory)
  (flet ((make (place)
           (funcall factory place :initial-alternative-type 't/detail/presentation)))
    (node/widget (:expanded #f)
        name
      (component-demo/widget "Special variable"
        (make (make-special-variable-place '*person-name* :type 'string)))
      (component-demo/widget "Lexical variable"
        (bind ((person-name "Kate"))
          (make (make-lexical-variable-place person-name :type 'string))))
      (component-demo/widget "Functional"
        (bind ((person-name "Mary"))
          (make (make-functional-place 'person-name
                                       (lambda ()
                                         person-name)
                                       (lambda (new-value)
                                         (setf person-name new-value))
                                       :type 'string))))
      (component-demo/widget "Simple functional"
        (make (make-simple-functional-place (cons "Steve" "Kate") 'car :type 'string)))
      (component-demo/widget "Sequence element"
        (make (make-sequence-element-place *person-name* 0)))
      (component-demo/widget "Instance slot"
        (make (make-object-slot-place (make-instance 'action :id "George") 'hu.dwim.wui::id))))))

;;;;;;
;;; Object

(def function make-object-makers-node ()
  (make-object-presentations-node "Maker" 'make-maker))

(def function make-object-viewers-node ()
  (make-object-presentations-node "Viewer" 'make-viewer))

(def function make-object-editors-node ()
  (make-object-presentations-node "Editor" 'make-editor))

(def function make-object-inspectors-node ()
  (make-object-presentations-node "Inspector" 'make-inspector))

(def function make-object-filters-node ()
  (make-object-presentations-node "Filter" 'make-filter))

(def function make-object-presentations-node (name factory)
  (node/widget (:expanded #f)
      name
    ))

;;;;;;
;;; Object sequence

(def function make-object-sequence-makers-node ()
  (make-object-sequence-presentations-node "Maker" 'make-maker))

(def function make-object-sequence-viewers-node ()
  (make-object-sequence-presentations-node "Viewer" 'make-viewer))

(def function make-object-sequence-editors-node ()
  (make-object-sequence-presentations-node "Editor" 'make-editor))

(def function make-object-sequence-inspectors-node ()
  (make-object-sequence-presentations-node "Inspector" 'make-inspector))

(def function make-object-sequence-filters-node ()
  (make-object-sequence-presentations-node "Filter" 'make-filter))

(def function make-object-sequence-presentations-node (name factory)
  (node/widget (:expanded #f)
      name
    ))

;;;;;;
;;; Object tree

(def function make-object-tree-makers-node ()
  (make-object-tree-presentations-node "Maker" 'make-maker))

(def function make-object-tree-viewers-node ()
  (make-object-tree-presentations-node "Viewer" 'make-viewer))

(def function make-object-tree-editors-node ()
  (make-object-tree-presentations-node "Editor" 'make-editor))

(def function make-object-tree-inspectors-node ()
  (make-object-tree-presentations-node "Inspector" 'make-inspector))

(def function make-object-tree-filters-node ()
  (make-object-tree-presentations-node "Filter" 'make-filter))

(def function make-object-tree-presentations-node (name factory)
  (node/widget (:expanded #f)
      name
    ))

;;;;;;
;;; Book

(def (constant :test 'string=) +lorem-ipsum+ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla metus eros, vestibulum eu scelerisque sed, lacinia at neque. In vulputate, erat imperdiet vestibulum commodo, risus diam pretium risus, quis ullamcorper urna velit eget ligula. Quisque egestas laoreet neque, id varius lacus tempus at. Sed varius vulputate dui a cursus. Fusce eleifend pulvinar purus, eu iaculis leo adipiscing nec. Proin pellentesque cursus felis, vitae faucibus ante interdum sed. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed est velit, blandit sollicitudin pulvinar at, pretium ac mi. Etiam molestie dictum massa id elementum. In bibendum, ligula cursus sagittis convallis, erat urna bibendum neque, ut pulvinar elit leo vitae metus. Vivamus id eleifend elit. Vestibulum et odio ipsum. Cras tellus turpis, malesuada facilisis tristique tincidunt, semper sollicitudin neque. In ornare turpis non lorem feugiat dapibus.")

(def function make-book-elements-node ()
  (node/widget (:expanded #f)
      "Book"
    (component-demo/widget "Book"
      (make-value-inspector
       (book (:title "The Guide")
         +lorem-ipsum+)
       :initial-alternative-type 't/text/inspector))
    (component-demo/widget "Chapter"
      (make-value-inspector
       (chapter (:title "Lorem ipsum")
         +lorem-ipsum+)
       :initial-alternative-type 't/text/inspector))
    (component-demo/widget "Glossary"
      "TODO")
    (component-demo/widget "Index"
      "TODO")
    (component-demo/widget "Paragraph"
      (make-value-inspector
       (paragraph ()
         +lorem-ipsum+)
       :initial-alternative-type 't/text/inspector))
    (component-demo/widget "Toc"
      "TODO")))

;;;;;;
;;; Source

(def function make-source-elements-node ()
  (node/widget (:expanded #f)
      "Source"
    (component-demo/widget "System"
      (make-value-inspector (asdf:find-system :hu.dwim.wui)))
    (component-demo/widget "Module"
      (make-value-inspector (reduce 'asdf:find-component (list "src" "component") :initial-value (asdf:find-system :hu.dwim.wui.component))))
    (component-demo/widget "Source file"
      (make-value-inspector (reduce 'asdf:find-component (list "src" "component" "api" "api") :initial-value (asdf:find-system :hu.dwim.wui.component))))
    (component-demo/widget "Text file"
      (pathname/text-file/inspector (asdf:system-relative-pathname :hu.dwim.wui "README")))
    (component-demo/widget "Pathname"
      (make-value-inspector (system-relative-pathname :hu.dwim.wui.component "src/component/api/api.lisp")))
    (component-demo/widget "Package"
      (make-value-inspector (find-package :hu.dwim.wui)))
    (component-demo/widget "Dictionary"
      (make-value-inspector (find-dictionary 'render-dictionary)))
    (component-demo/widget "Defintion name"
      (symbol/definition-name/inspector ()
        'render-component))
    (component-demo/widget "Special variable name"
      (symbol/special-variable-name/inspector ()
        '*xml-stream*))
    (component-demo/widget "Type name"
      (symbol/type-name/inspector ()
        'components))
    (component-demo/widget "Structure class"
      (make-value-inspector (find-class 'package)))
    (component-demo/widget "Structure direct slot definition"
      (make-value-inspector (first (class-direct-slots (find-class 'package)))))
    (component-demo/widget "Structure effective slot definition"
      (make-value-inspector (first (class-slots (find-class 'package)))))
    (component-demo/widget "Standard class"
      (make-value-inspector (find-class 'parent/mixin)))
    (component-demo/widget "Standard direct slot definition"
      (make-value-inspector (first (class-direct-slots (find-class 'parent/mixin)))))
    (component-demo/widget "Standard effective slot definition"
      (make-value-inspector (first (class-slots (find-class 'parent/mixin)))))
    (component-demo/widget "Lisp form"
      (t/lisp-form/inspector ()
        ｢;; a simple example
         (defun foo ()
           (print `(#f #t 42 3.14 #\a "Hello World" #(1 2) :foo bar #'list ,42)))｣))
    (component-demo/widget "Function"
      (make-value-inspector (fdefinition 'make-value-inspector)))
    (component-demo/widget "Standard generic function"
      (make-value-inspector (fdefinition 'make-instance)))
    (component-demo/widget "Standard method"
      (make-value-inspector (second (generic-function-methods (fdefinition 'handle-request)))))
    (component-demo/widget "Test"
      (make-value-inspector (hu.dwim.stefil::find-test 'test)))))

;;;;;;
;;; Chart

(def function make-charts-node ()
  (node/widget (:expanded #f)
      "Chart"
    (component-demo/widget "Column"
      (column/chart (:title "Salary"
                     :width 400
                     :height 400)
        ("John" 12500)
        ("Mary" 14300)
        ("Steve" 9800)
        ("Kate" 13700)))
    (component-demo/widget "Flow"
      "TODO")
    (component-demo/widget "Line"
      "TODO")
    (component-demo/widget "Pie"
      (pie/chart (:title "Salary"
                  :width 400
                  :height 400)
        ("John" 12500)
        ("Mary" 14300)
        ("Steve" 9800)
        ("Kate" 13700)))
    (component-demo/widget "Radar"
      "TODO")
    (component-demo/widget "Scatter"
      "TODO")
    (component-demo/widget "Stock"
      "TODO")
    (component-demo/widget "Structure"
      "TODO")))

;;;;;;
;;; Demo

(def function make-demo-content (&optional initial-content-component)
  (bind ((content (content/widget ()
                    (or initial-content-component
                        (empty/layout)))))
    (target-place/widget (:target-place (make-object-slot-place content 'hu.dwim.wui::content))
      (horizontal-list/layout ()
        (tree/widget ()
          (node/widget ()
              "Component"
            (make-immediates-node)
            (make-layouts-node)
            (make-widgets-node)
            (node/widget (:expanded #f)
                "Primitive"
              (make-primitive-makers-node)
              (make-primitive-viewers-node)
              (make-primitive-editors-node)
              (make-primitive-inspectors-node)
              (make-primitive-filters-node))
            (node/widget (:expanded #f)
                "Place"
              (make-place-makers-node)
              (make-place-viewers-node)
              (make-place-editors-node)
              (make-place-inspectors-node)
              (make-place-filters-node))
            (node/widget (:expanded #f)
                "Object"
              (make-object-makers-node)
              (make-object-viewers-node)
              (make-object-editors-node)
              (make-object-inspectors-node)
              (make-object-filters-node))
            (node/widget (:expanded #f)
                "Object sequence"
              (make-object-sequence-makers-node)
              (make-object-sequence-viewers-node)
              (make-object-sequence-editors-node)
              (make-object-sequence-inspectors-node)
              (make-object-sequence-filters-node))
            (node/widget (:expanded #f)
                "Object tree"
              (make-object-tree-makers-node)
              (make-object-tree-viewers-node)
              (make-object-tree-editors-node)
              (make-object-tree-inspectors-node)
              (make-object-tree-filters-node))
            (make-book-elements-node)
            (make-source-elements-node)
            (make-charts-node)))
        content))))
