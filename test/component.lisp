;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui.test)

;;;;;;
;;; This is a simple example application with various components

(eval-always
  (def function make-stylesheet-uri (path)
    (list (string+ "static/" path)
          (assert-file-exists (system-relative-pathname :hu.dwim.wui.test (string+ "www/" path))))))

(def constant +demo-static-stylesheet-uris+ (list (make-stylesheet-uri "wui/css/wui.css")
                                                  (make-stylesheet-uri "wui/css/icon.css")
                                                  (make-stylesheet-uri "wui/css/border.css")
                                                  (make-stylesheet-uri "wui/css/layout.css")
                                                  (make-stylesheet-uri "wui/css/widget.css")
                                                  (make-stylesheet-uri "wui/css/text.css")
                                                  (make-stylesheet-uri "wui/css/lisp-form.css")
                                                  (make-stylesheet-uri "wui/css/shell-script.css")
                                                  (make-stylesheet-uri "wui/css/presentation.css")
                                                  (make-stylesheet-uri "wui/css/test.css")))

(def constant +demo-script-uris+ '("wui/js/wui.js" "wui/js/component-hierarchy.js"))

(def constant +demo-page-icon+ "static/favicon.ico")

(def special-variable *component-demo-application*
  (make-instance 'test-application
                 :path-prefix "/"
                 :home-package (find-package :hu.dwim.wui.test)
                 :default-locale "en"
                 :frame-root-component-factory 'make-demo-frame-component-with-content
                 :dojo-directory-name (find-latest-dojo-directory-name (system-relative-pathname :hu.dwim.wui "www/"))
                 :ajax-enabled #t))

;;;;;;
;;; Entry point

(def file-serving-entry-point *component-demo-application* "static/" (system-relative-pathname :hu.dwim.wui "www/"))

(def js-file-serving-entry-point *component-demo-application* "wui/js/" (system-relative-pathname :hu.dwim.wui "source/js/"))

(def js-component-hierarchy-serving-entry-point *component-demo-application* "wui/js/component-hierarchy.js")

(def entry-point (*component-demo-application* :path "" :ensure-session #t :ensure-frame #t) ()
  (assert (and (boundp '*session*) *session*))
  (assert (and (boundp '*frame*) *frame*))
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*) (make-demo-frame-component))
        (make-redirect-response-for-current-application))))

(def function start-test-server-with-component-demo-application (&key (maximum-worker-count 16) (log-level +debug+) (host *test-host*) (port *test-port*))
  (setf (log-level 'wui) log-level)
  (start-test-server-with-brokers (list *component-demo-application*
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
                 :stylesheet-uris (append (list (make-stylesheet-uri (string+ *dojo-directory-name* "dojo/resources/dojo.css"))
                                                (make-stylesheet-uri (string+ *dojo-directory-name* "dijit/themes/tundra/tundra.css")))
                                          +demo-static-stylesheet-uris+)
                 :script-uris +demo-script-uris+
                 :page-icon +demo-page-icon+)
    (top/widget (:menu-bar (menu-bar/widget ()
                             (make-debug-menu)))
      (make-component-demo-content initial-content-component))))

;;;;;;
;;; Immediate

(def function make-immediates-node ()
  (node/widget (:expanded #f)
      (replace-target-place/widget ()
          "Immediate"
        (make-value-viewer (first (make-definitions 'component/immediate))))
    (component-demo/widget "Number"
      42)
    (component-demo/widget "String"
      "John")))

;;;;;
;;; Layout

(def function make-layouts-node ()
  (node/widget (:expanded #f)
      (replace-target-place/widget ()
          "Layout"
        (make-value-inspector (find-class 'layout/abstract)))
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
      ;; NOTE: see #container in test.css
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
      (replace-target-place/widget ()
          "Widget"
        (make-value-inspector (find-class 'widget/abstract)))
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
      (tab-container/widget ()
        (tab-page/widget (:selector (icon switch-to-tab-page :label "Male"))
          "John")
        (tab-page/widget (:selector (icon switch-to-tab-page :label "Female"))
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
    (component-demo/widget "Page navigation bar"
      (page-navigation-bar/widget :total-count 100))
    (component-demo/widget "Push button"
      (push-button/widget ()
        (command/widget ()
          (icon refresh-component)
          (make-action))))
    (component-demo/widget "Toggle button"
      (toggle-button/widget ()
        (command/widget ()
          (icon refresh-component)
          (make-action))))
    (component-demo/widget "Drop down button"
      (drop-down-button/widget ()
        (command/widget ()
          (icon refresh-component)
          (make-action))))
    (component-demo/widget "File download"
      (download-file/widget :file-name (system-relative-pathname :hu.dwim.wui "test/component.lisp")))
    (component-demo/widget "File upload"
      (upload-file/widget))
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
    (component-demo/widget "Name value pair"
      (name-value-pair/widget ()
        "First Name"
        "John"))
    (component-demo/widget "Name value group"
      (name-value-group/widget (:title "Name")
        (name-value-pair/widget ()
          "First Name"
          "John")
        (name-value-pair/widget ()
          "Last Name"
          "Doe")))
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
        "John"
        "Mary"
        "Steve"
        "Kate"))
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
          "Unfriendly")))
    (component-demo/widget "Wizard"
      (wizard/widget ()
        "John"
        "Mary"
        "Steve"
        "Kate"))))

;;;;;;
;;; Primitive

(def function make-primitive-node ()
  (node/widget (:expanded #f)
      (replace-target-place/widget ()
          "Primitive"
        (make-value-inspector (find-class 'primitive/presentation)))
    (make-primitive-makers-node)
    (make-primitive-viewers-node)
    (make-primitive-editors-node)
    (make-primitive-inspectors-node)
    (make-primitive-filters-node)))

;; NOTE: all this hassle is to have a reasonable code shown by component-demo/widget
(def macro make-primitive-presentations-node (name factory supercomponent)
  (flet ((make (type value)
           `(,factory ',type :value ,value)))
    `(node/widget (:expanded #f)
         (replace-target-place/widget ()
             ,name
           (make-value-inspector (find-class ',supercomponent)))
       (component-demo/widget "Null"
         ,(make 'null nil))
       (component-demo/widget "Boolean"
         ,(make 'boolean #t))
       (component-demo/widget "Bit"
         ,(make 'bit 0))
       (component-demo/widget "Character"
         ,(make 'character  #\J))
       (component-demo/widget "String"
         ,(make 'string  "John"))
       (component-demo/widget "Password"
         ,(make 'password "Mary"))
       (component-demo/widget "Symbol"
         ,(make 'symbol ''John))
       (component-demo/widget "Keyword"
         ,(make 'keyword ':value))
       (component-demo/widget "Number"
         ,(make 'number 12345678901234567890/10000000000))
       (component-demo/widget "Real"
         ,(make 'real 3.14))
       (component-demo/widget "Integer"
         ,(make 'integer 42))
       (component-demo/widget "Rational"
         ,(make 'rational 31/7))
       (component-demo/widget "Float"
         ,(make 'float 42.42))
       (component-demo/widget "Complex"
         ,(make 'complex #C(42 -42)))
       (component-demo/widget "Date"
         ,(make 'local-time:date (aprog1 (local-time:now)
                                   (setf (local-time:nsec-of it) 0
                                         (local-time:sec-of it) 0))))
       (component-demo/widget "Time"
         ,(make 'local-time:time (aprog1 (local-time:now)
                                   (setf (local-time:day-of it) 0))))
       (component-demo/widget "Timestamp"
         ,(make 'local-time:timestamp (local-time:now)))
       (component-demo/widget "Member"
         ,(make '(member John Mary Steve Kate) ''John))
       (component-demo/widget "HTML"
         ,(make 'html-text "John <b>Mary</b> <h2>Steve</h2> <i>Kate</i>"))
       (component-demo/widget "IP address"
         ,(make 'iolib.sockets:inet-address (iolib:ensure-address "127.0.0.1")))
       ;; TODO: what is this type supposed to be? a slot type for a download/upload widget...
       #+nil
       (component-demo/widget "File"
         ,(make 'file (system-relative-pathname :hu.dwim.wui "test/component.lisp"))))))

(def function make-primitive-makers-node ()
  (make-primitive-presentations-node "Maker" make-maker primitive/maker))

(def function make-primitive-viewers-node ()
  (make-primitive-presentations-node "Viewer" make-viewer primitive/viewer))

(def function make-primitive-editors-node ()
  (make-primitive-presentations-node "Editor" make-editor primitive/editor))

(def function make-primitive-inspectors-node ()
  (make-primitive-presentations-node "Inspector" make-inspector primitive/inspector))

(def function make-primitive-filters-node ()
  (make-primitive-presentations-node "Filter" make-filter primitive/filter))

;;;;;;
;;; Place

(def special-variable *person-name* "John")

(def function make-place-node ()
  (node/widget (:expanded #f)
      (replace-target-place/widget ()
          "Place"
        (make-value-inspector (find-class 'place/presentation)))
    (make-place-makers-node)
    (make-place-viewers-node)
    (make-place-editors-node)
    (make-place-inspectors-node)
    (make-place-filters-node)))

;; NOTE: all this hassle is to have a reasonable code shown by component-demo/widget
(def macro make-place-presentations-node (name factory supercomponent)
  (flet ((make (place)
           `(,factory 'place :value ,place)))
    `(node/widget (:expanded #f)
         (replace-target-place/widget ()
             ,name
           (make-value-inspector (find-class ',supercomponent)))
       (component-demo/widget "Special variable"
         ,(make '(make-special-variable-place '*person-name* :type 'string)))
       (component-demo/widget "Lexical variable"
         ,(make '(bind ((person-name "Kate"))
                  (make-lexical-variable-place person-name :type 'string))))
       (component-demo/widget "Functional"
         ,(make '(bind ((person-name "Mary"))
                  (make-functional-place 'person-name
                   (lambda ()
                     person-name)
                   (lambda (new-value)
                     (setf person-name new-value))
                   :type 'string))))
       (component-demo/widget "Simple functional"
         ,(make '(make-simple-functional-place (cons "Steve" "Kate") 'car :type 'string)))
       (component-demo/widget "Sequence element"
         ,(make '(make-sequence-element-place *person-name* 0)))
       (component-demo/widget "Instance slot"
         ,(make '(make-object-slot-place (make-instance 'action :id "George") 'hu.dwim.wui::id))))))

(def function make-place-makers-node ()
  (make-place-presentations-node "Maker" make-maker place/maker))

(def function make-place-viewers-node ()
  (make-place-presentations-node "Viewer" make-viewer place/viewer))

(def function make-place-editors-node ()
  (make-place-presentations-node "Editor" make-editor place/editor))

(def function make-place-inspectors-node ()
  (make-place-presentations-node "Inspector" make-inspector place/inspector))

(def function make-place-filters-node ()
  (make-place-presentations-node "Filter" make-filter place/filter))

;;;;;;
;;; Object

(def function make-object-node ()
  (node/widget (:expanded #f)
      (replace-target-place/widget ()
          "Object"
        (make-value-inspector (find-class 't/presentation)))
    (make-object-makers-node)
    (make-object-viewers-node)
    (make-object-editors-node)
    (make-object-inspectors-node)
    (make-object-filters-node)))

(def macro make-object-presentations-node (name factory supercomponent)
  (flet ((make (type value)
           `(,factory ',type :value ,value)))
    `(node/widget (:expanded #f)
         (replace-target-place/widget ()
             ,name
           (make-value-inspector (find-class ',supercomponent)))
       (component-demo/widget "Null"
         ,(make '(or null standard-object) nil))
       (component-demo/widget "Standard object"
         ,(make 'standard-object '(make-instance 'standard-object)))
       (component-demo/widget "Server"
         ,(make 'server '*server*))
       (component-demo/widget "Application"
         ,(make 'application '*application*))
       (component-demo/widget "Session"
         ,(make 'session '*session*))
       (component-demo/widget "Frame"
         ,(make 'frame '*frame*))
       (component-demo/widget "Request"
         ,(make 'request '*request*))
       (component-demo/widget "Response"
         ,(make 'response '*response*)))))

(def function make-object-makers-node ()
  (make-object-presentations-node "Maker" make-maker t/maker))

(def function make-object-viewers-node ()
  (make-object-presentations-node "Viewer" make-viewer t/viewer))

(def function make-object-editors-node ()
  (make-object-presentations-node "Editor" make-editor t/editor))

(def function make-object-inspectors-node ()
  (make-object-presentations-node "Inspector" make-inspector t/inspector))

(def function make-object-filters-node ()
  (make-object-presentations-node "Filter" make-filter t/filter))

;;;;;;
;;; Sequence

(def function make-sequence-node ()
  (node/widget (:expanded #f)
      (replace-target-place/widget ()
          "Sequence"
        (make-value-inspector (find-class 'sequence/presentation)))
    (make-sequence-makers-node)
    (make-sequence-viewers-node)
    (make-sequence-editors-node)
    (make-sequence-inspectors-node)
    (make-sequence-filters-node)))

(def macro make-sequence-presentations-node (name factory supercomponent)
  (flet ((make (type value)
           `(,factory ',type :value ,value)))
    `(node/widget (:expanded #f)
         (replace-target-place/widget ()
             ,name
           (make-value-inspector (find-class ',supercomponent)))
       (component-demo/widget "Nil"
         ,(make 'sequence nil))
       (component-demo/widget "List"
         ,(make 'list ''("John" "Mary" "Steve" "Kate")))
       (component-demo/widget "Vector"
         ,(make 'vector #("Kate" "Steve" "Mary" "John"))))))

(def function make-sequence-makers-node ()
  (make-sequence-presentations-node "Maker" make-maker sequence/maker))

(def function make-sequence-viewers-node ()
  (make-sequence-presentations-node "Viewer" make-viewer sequence/viewer))

(def function make-sequence-editors-node ()
  (make-sequence-presentations-node "Editor" make-editor sequence/editor))

(def function make-sequence-inspectors-node ()
  (make-sequence-presentations-node "Inspector" make-inspector sequence/inspector))

(def function make-sequence-filters-node ()
  (make-sequence-presentations-node "Filter" make-filter sequence/filter))

;;;;;;
;;; Tree

(def function make-tree-node ()
  (node/widget (:expanded #f)
      "Tree"
    (make-tree-makers-node)
    (make-tree-viewers-node)
    (make-tree-editors-node)
    (make-tree-inspectors-node)
    (make-tree-filters-node)))

(def macro make-tree-presentations-node (name factory supercomponent)
  (flet ((make (type value)
           `(,factory ',type :value ,value)))
    `(node/widget (:expanded #f)
         (replace-target-place/widget ()
             ,name
           (make-value-inspector (find-class ',supercomponent)))
       )))

(def function make-tree-makers-node ()
  (make-tree-presentations-node "Maker" make-maker nil))

(def function make-tree-viewers-node ()
  (make-tree-presentations-node "Viewer" make-viewer nil))

(def function make-tree-editors-node ()
  (make-tree-presentations-node "Editor" make-editor nil))

(def function make-tree-inspectors-node ()
  (make-tree-presentations-node "Inspector" make-inspector nil))

(def function make-tree-filters-node ()
  (make-tree-presentations-node "Filter" make-filter nil))

;;;;;;
;;; Book

(def constant +lorem-ipsum+ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla metus eros, vestibulum eu scelerisque sed, lacinia at neque. In vulputate, erat imperdiet vestibulum commodo, risus diam pretium risus, quis ullamcorper urna velit eget ligula. Quisque egestas laoreet neque, id varius lacus tempus at. Sed varius vulputate dui a cursus. Fusce eleifend pulvinar purus, eu iaculis leo adipiscing nec. Proin pellentesque cursus felis, vitae faucibus ante interdum sed. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed est velit, blandit sollicitudin pulvinar at, pretium ac mi. Etiam molestie dictum massa id elementum. In bibendum, ligula cursus sagittis convallis, erat urna bibendum neque, ut pulvinar elit leo vitae metus. Vivamus id eleifend elit. Vestibulum et odio ipsum. Cras tellus turpis, malesuada facilisis tristique tincidunt, semper sollicitudin neque. In ornare turpis non lorem feugiat dapibus.")

(def function make-book-elements-node ()
  (node/widget (:expanded #f)
      "Text"
    (component-demo/widget "Book"
      (make-value-inspector
       (book (:title "The Guide" :authors '("Levente Mészáros"))
         +lorem-ipsum+)))
    (component-demo/widget "Chapter"
      (make-value-inspector
       (chapter (:title "Lorem ipsum")
         +lorem-ipsum+)))
    (component-demo/widget "Glossary"
      "TODO")
    (component-demo/widget "Index"
      "TODO")
    (component-demo/widget "Paragraph"
      (make-value-inspector
       (paragraph ()
         +lorem-ipsum+)))
    (component-demo/widget "Toc"
      "TODO")
    (component-demo/widget "URI"
      (make-value-inspector (parse-uri "http://dwim.hu/")))))

;;;;;;
;;; Source

(def function make-source-elements-node ()
  (node/widget (:expanded #f)
      "Source"
    (component-demo/widget "System"
      (make-value-inspector (asdf:find-system :hu.dwim.wui)))
    (component-demo/widget "Module"
      (make-value-inspector (reduce 'asdf:find-component (list "source" "component") :initial-value (asdf:find-system :hu.dwim.wui.component))))
    (component-demo/widget "Source file"
      (make-value-inspector (system-relative-pathname :hu.dwim.wui.component "source/component/api/api.lisp")))
    (component-demo/widget "Text file"
      (make-value-inspector (asdf:system-relative-pathname :hu.dwim.wui "LICENCE")))
    (component-demo/widget "Binary file"
      (make-value-inspector (asdf:system-relative-pathname :hu.dwim.wui "www/wui/icon/10x10/arrow-out.png")))
    (component-demo/widget "Pathname"
      (make-value-inspector (system-relative-pathname :hu.dwim.wui.component "")))
    (component-demo/widget "Package"
      (make-value-inspector (find-package :hu.dwim.wui)))
    (component-demo/widget "Dictionary"
      (make-value-inspector (make-instance 'dictionary
                                           :name 'editing
                                           :definition-names '(editable/mixin begin-editing save-editing cancel-editing store-editing revert-editing join-editing leave-editing))))
    (component-demo/widget "Definition"
      (make-value-inspector (make-definitions 'list)))
    (component-demo/widget "Special variable"
      (make-value-inspector (first (make-definitions '*xml-stream*))))
    (component-demo/widget "Type"
      (make-value-inspector (first (make-definitions 'components))))
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
      (make-value-inspector (symbol-function 'make-value-inspector)))
    (component-demo/widget "Standard generic function"
      (make-value-inspector (symbol-function 'make-instance)))
    (component-demo/widget "Standard method"
      (make-value-inspector (second (generic-function-methods (symbol-function 'handle-request)))))
    (component-demo/widget "Macro"
      (make-value-inspector (first (make-definitions 'with-lock-held-on-application))))
    (component-demo/widget "Test"
      (make-value-inspector (hu.dwim.stefil::find-test 'test)))
    (component-demo/widget "Shell script"
      (make-value-inspector (shell-script ()
                              "sudo apt-get install clisp"
                              "cd ~/workspace/sbcl"
                              "wget http://dwim.hu/install/customize-target-features.lisp"
                              "sh ~/workspace/sbcl/make.sh \"clisp -ansi -on-error abort\""
                              "sudo sh ~/workspace/sbcl/install.sh")))))

;;;;;;
;;; Chart

(def function make-charts-node ()
  (node/widget (:expanded #f)
      (replace-target-place/widget ()
          "Chart"
        (make-value-inspector (find-class 'chart/abstract)))
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
;;; Processes

(def function make-processes-node ()
  (node/widget (:expanded #f)
      (replace-target-place/widget ()
          "Process"
        (make-value-inspector (find-class 'standard-process/user-interface/inspector)))
    (component-demo/widget "Empty"
      (make-value-inspector (standard-process)))
    (component-demo/widget "Literal"
      (make-value-inspector (standard-process
                              42)))
    (component-demo/widget "Call"
      (make-value-inspector (standard-process
                              (call-component "Hello World" (answer/widget ()
                                                                (icon answer-component :label "Finish"))))))
    (component-demo/widget "Sequential"
      (make-value-inspector (standard-process
                              (call-component "John" (answer/widget ()
                                                         (icon answer-component :label "Next")))
                              (call-component "Mary" (answer/widget ()
                                                         (icon answer-component :label "Next")))
                              (call-component "Steve" (answer/widget ()
                                                          (icon answer-component :label "Next")))
                              (call-component "Kate" (answer/widget ()
                                                         (icon answer-component :label "Finish"))))))
    (component-demo/widget "Loop"
      (make-value-inspector (standard-process
                              (iter (with max = 4)
                                    (for i :from 0 :below max)
                                    (for label = (if (= i (1- max)) "Finish" "Next"))
                                    (call-component (format nil "At ~A" i)
                                                    (answer/widget ()
                                                        (icon answer-component :label label)))))))
    (component-demo/widget "Branch"
      (make-value-inspector (standard-process
                              (ecase (call-component "John"
                                                     (list (answer/widget ()
                                                               (icon answer-component :label "Male")
                                                             :male)
                                                           (answer/widget ()
                                                               (icon answer-component :label "Female")
                                                             :female)))
                                (:male (call-component "Steve"
                                                       (answer/widget ()
                                                           (icon answer-component :label "Finish"))))
                                (:female (call-component "Kate"
                                                         (answer/widget ()
                                                             (icon answer-component :label "Finish"))))))))
    (component-demo/widget "Recursive branch"
      (make-value-inspector (standard-process
                              (recursive-branch "Root" 4))))))

(def function/cc recursive-branch (label level)
  (unless (zerop level)
    (recursive-branch (format nil "~A.~A" label
                              (call-component label
                                              (if (= level 1)
                                                  (answer/widget () (icon answer-component :label "Finish"))
                                                  (list (answer/widget () (icon answer-component :label "0") 0)
                                                        (answer/widget () (icon answer-component :label "1") 1)))))
                      (1- level))))

;;;;;;
;;; Customizations

(def function make-customization-node ()
  (node/widget (:expanded #f)
      (replace-target-place/widget ()
          "Customization"
        (content/widget ()
          "A couple of somewhat more complex examples that demonstrate how to combine the components."))
    (component-demo/widget "Inspector"
      (table/widget (:columns (list (column/widget ()
                                      "Foo")
                                    (column/widget ()
                                      "Bar")))
        (row/widget ()
          (cell/widget ()
            (make-instance 'place/value/inspector :component-value (make-object-slot-place *request* 'hu.dwim.wui::http-method)))
          (cell/widget ()
            (make-instance 'place/value/inspector :component-value (make-object-slot-place *application* 'hu.dwim.wui::frame-timeout))))
        (row/widget ()
          (cell/widget ()
            (make-instance 'place/value/inspector :component-value (make-object-slot-place *frame* 'hu.dwim.wui::action-id->action)))
          (cell/widget ()
            (make-instance 'place/value/inspector :component-value (make-object-slot-place *server* 'hu.dwim.wui::started-at))))))
    (component-demo/widget "Filter"
      "TODO")))

;;;;;;
;;; Demo

(def (function e) make-component-demo-content (&optional initial-content-component)
  (bind ((content (content/widget ()
                    (or initial-content-component
                        (empty/layout)))))
    (target-place/widget (:target-place (make-object-slot-place content 'hu.dwim.wui::content))
      (horizontal-list/layout ()
        (tree/widget ()
          (node/widget ()
              (replace-target-place/widget ()
                  "Component"
                (make-value-inspector (find-class 'component)))
            (make-immediates-node)
            (make-layouts-node)
            (make-widgets-node)
            (make-primitive-node)
            (make-place-node)
            (make-object-node)
            (make-sequence-node)
            (make-tree-node)
            (make-book-elements-node)
            (make-source-elements-node)
            (make-charts-node)
            (make-processes-node)
            (make-customization-node)))
        content))))
