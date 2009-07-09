(in-package :wui.test)

;;;;;;
;;; This is a simple example application with login and logout support

(setf *dojo-directory-name* (find-latest-dojo-directory-name (project-relative-pathname "wwwroot/")))

(def special-variable *demo-stylesheet-uris* (append (flet ((entry (path)
                                                              (list (concatenate-string "static/" path)
                                                                    (assert-file-exists (project-relative-pathname (concatenate-string "wwwroot/" path)))))
                                                            (dojo-relative-path (path)
                                                              (concatenate-string *dojo-directory-name* path)))
                                                       (list (entry "wui/css/demo.css")
                                                             (entry "wui/css/wui.css")
                                                             (entry "wui/css/wui-default-skin.css")
                                                             (entry (dojo-relative-path "dijit/themes/tundra/tundra.css"))
                                                             (entry (dojo-relative-path "dojo/resources/dojo.css"))))))

(def (constant :test 'equalp) +demo-script-uris+ '("wui/js/wui.js"))

(def (constant :test 'string=) +demo-page-icon+ "static/favicon.ico")

(def constant +minimum-login-password-length+ 6)
(def constant +minimum-login-identifier-length+ 3)

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
                 :home-package (find-package :wui.test)
                 :default-locale "en"
                 ;; force ajax eanbled regardless *default-ajax-enabled*
                 :ajax-enabled t))

(def layered-method make-frame-component-with-content ((application demo-application) session frame content)
  (make-demo-frame-component-with-content content))

#|

;;;;;;
;;; the factories that create the component graph both in logged out and logged in states

(def function make-demo-frame-component ()
  (make-demo-frame-component-with-content (make-demo-menu-content-component)))

(def function make-demo-frame-component-with-content (menu-content)
  (bind ((menu (make-demo-menu-component)))
    (bind ((frame-component (frame (:title "demo"
                                    :stylesheet-uris *demo-stylesheet-uris*
                                    :script-uris '#.+demo-script-uris+
                                    :page-icon #.+demo-page-icon+)
                              (vertical-list (:id "page")
                                (when *session*
                                  (style (:id "header" :css-class "authenticated")
                                    (inline-render-component/widget ()
                                      <span ,(current-authenticated-subject)
                                            " "
                                            ,(when (running-in-test-mode-p *demo-application*)
                                                   "(test mode)")
                                            " "
                                            ,(when (profile-request-processing-p *server*)
                                                   "(profiling)")
                                            " "
                                            ,(when (debug-client-side? (root-component-of *frame*))
                                                   "(debugging client side)")>
                                      <span ,(render-component
                                              (command (icon logout)
                                                (make-action
                                                  (mark-session-invalid *session*)
                                                  (make-redirect-response-for-current-application))))>)))
                                (horizontal-list ()
                                  (style (:id "menu") menu)
                                  (top menu-content))))))
      (setf (target-place-of menu) (make-component-place menu-content))
      (values frame-component menu-content))))

(def function make-demo-menu-component ()
  (if (is-authenticated?)
      (make-authenticated-menu-component)
      (make-unauthenticated-menu-component)))

(def function make-demo-menu-content-component ()
  (if (is-authenticated?)
      (empty)
      (if (parameter-value "example-error")
          (inline-render-component/widget ()
            (error "This is an example error that happens while rendering the response"))
          (bind ((path (path-of (uri-of *request*))))
            (assert (starts-with-subseq (path-prefix-of *application*) path))
            (setf path (subseq path (length (path-prefix-of *application*))))
            (switch (path :test #'string=)
              (+login-entry-point-path+ (vertical-list ()
                                          (make-identifier-and-password-login-component)
                                          (inline-render-component/widget ()
                                            <div "FYI, the password is \"secret\"...">)))
              ("help/" (make-help-component))
              ("about/" (make-about-component))
              (t (inline-render-component/widget ()
                   <span "demo start page">)))))))

(def function make-unauthenticated-menu-component ()
  (macrolet ((command* (title path)
               `(command ,title
                         ,(if (stringp path)
                              `(make-application-relative-uri ,path)
                              path)
                         :send-client-state #f)))
    (menu
      (menu-item () (command* #"menu.front-page"              ""))
      (menu-item () (command* #"menu.help"                    "help/"))
      (menu-item () (command* #"menu.login"                   #.+login-entry-point-path+))
      (menu-item () (command* #"menu.about"                   "about/"))
      (menu-item () (command* #"menu.example-rendering-error" (bind ((uri (make-application-relative-uri "")))
                                                                (setf (uri-query-parameter-value uri "example-error") "t")
                                                                uri))))))

;; TODO the onChange part should be this simple, but it needs qq work.
;; ,(js-to-lisp-rpc
;;   (setf (example-inline-edit-box-value-of *session*) (aref |arguments| 0)))

(def function render-example-inline-edit-box ()
  `js(dojo.require #.+dijit/inline-edit-box+)
  (bind ((id (generate-frame-unique-string "inlineEditor")))
    <span
      "This is an InlineEditBox widget whose value is stored in the session (remains after a \"Start over\")"
      <br>
      "Click value to edit:"
      ,(render-dojo-widget (id)
        <span (:id ,id
               :dojoType #.+dijit/inline-edit-box+
               :autoSave false
               :onChange `js-inline(wui.io.xhr-post
                                    (create
                                     :content (create :example-inline-edit-box-value (aref arguments 0))
                                     :url ,(action/href (:delayed-content #t)
                                             (with-request-params (((value "exampleInlineEditBoxValue") "this is the default value; it's a bug!"))
                                               (setf (example-inline-edit-box-value-of *session*) value))
                                             ;; TODO this is only proof-of-concept quality for now... it'll just close the http socket
                                             (make-do-nothing-response))
                                     :load (lambda (response args)))))
          ,(example-inline-edit-box-value-of *session*)>)>))

(def function make-authenticated-menu-component ()
  (bind ((authenticated-subject (current-authenticated-subject)))
    (menu
      (when (> (length authenticated-subject) 0) ; just a random condition for demo purposes
        (bind ((debug-menu (make-debug-menu-item)))
          (appendf (menu-items-of debug-menu)
                   (list (menu-item () (command "Example error in action body"
                                         (make-action (error "This is an example error which is signaled when running the action body"))))
                         (menu-item () (replace-menu-target-command "Example error while rendering"
                                         (inline-render-component/widget ()
                                           (error "This is an example error which is signaled when rendering the root component of the current frame"))))))
          debug-menu))
      (menu-item () "Charts"
        (menu-item () "Charts from files"
          (macrolet ((make-chart-menu (name type settings-file data-file)
                       `(replace-menu-target-command ,name
                          (make-chart-from-files ',type
                                                 :settings-file (project-relative-pathname ,(concatenate-string "test/amCharts/examples/" settings-file))
                                                 :data-file (project-relative-pathname ,(concatenate-string "test/amCharts/examples/" data-file))))))
            (menu-item () (make-chart-menu "Column chart" column-chart
                                           "amcolumn/3d_stacked_bar_chart/amcolumn_settings.xml"
                                           "amcolumn/3d_stacked_bar_chart/amcolumn_data.txt"))
            (menu-item () (make-chart-menu "Line chart" line-chart
                                           "amline/stacked_area_chart/amline_settings.xml"
                                           "amline/stacked_area_chart/amline_data.xml"))
            (menu-item () (make-chart-menu "Pie chart" pie-chart
                                           "ampie/donut/ampie_settings.xml"
                                           "ampie/donut/ampie_data.txt"))
            (menu-item () (make-chart-menu "Radar chart" radar-chart
                                           "amradar/stacked/amradar_settings.xml"
                                           "amradar/stacked/amradar_data.xml"))
            (menu-item () (make-chart-menu "Stock chart" stock-chart
                                           "amstock/ohlc/amstock_settings.xml"
                                           "amstock/ohlc/data.csv"))
            (menu-item () (make-chart-menu "Xy chart" xy-chart
                                           "amxy/time_plot/amxy_settings.xml"
                                           "amxy/time_plot/amxy_data.xml")))))
      (make-primitive-component-menu)
      (menu-item () "Metagui"
        (menu-item () "Parent"
          (menu-item () (replace-menu-target-command "Make a parent" (make-maker 'parent-test)))
          (menu-item () (replace-menu-target-command "Search parents" (make-filter 'parent-test))))
        (menu-item () "Child"
          (menu-item () (replace-menu-target-command "Make a child" (make-maker 'child-test)))
          (menu-item () (replace-menu-target-command "Search children" (make-filter 'child-test)))))
      (menu-item () (replace-menu-target-command "File upload test"
                      (bind ((file-upload-component (make-instance 'file-upload-component)))
                        (vertical-list ()
                          file-upload-component
                          (command (icon upload)
                                   (make-action
                                     ;;(break "file in action: ~A" (component-value-of file-upload-component))
                                     ))
                          (inline-render-component/widget ()
                            (when (slot-boundp file-upload-component 'component-value)
                              (render-mime-part-details (component-value-of file-upload-component))))))))
      (menu-item () (replace-menu-target-command "Dojo InlineEditBox example"
                      (inline-render-component/widget ()
                        (render-example-inline-edit-box))))
      (menu-item () (replace-menu-target-command "checkbox"
                      (bind ((value1 #t)
                             (value2 #f))
                        (vertical-list ()
                          (inline-render-component/widget ()
                            (render-checkbox-field value1 :value-sink (lambda (value)
                                                                        (setf value1 value)))
                            (render-checkbox-field value2 :value-sink (lambda (value)
                                                                        (setf value2 value))))
                          (command (icon refresh)
                                   (make-action
                                     ;; nop, just rerender
                                     (values)))))))
      (menu-item () (replace-menu-target-command "Ajax counter"
                      (make-instance 'counter-component)))
      (menu-item () "Others"
        (menu-item () (replace-menu-target-command #"menu.help" (make-help-component)))
        (menu-item () (replace-menu-target-command #"menu.about" (make-about-component)))))))

(def function make-primitive-component-menu ()
  (labels ((make-primitive-menu-item-content (components)
             (inline-render-component/widget ()
               <div ,(render-component (command (icon wui::refresh)
                                                (make-action)))
                    <table ,(foreach (lambda (component)
                                       <tr ,(foreach (lambda (cell)
                                                       <td ,(render-component cell)>)
                                                     component)>)
                                     components)>>))
           (make-primitive-menu-item (name types values initforms)
             (menu-item () (string-capitalize (string-downcase (symbol-name name)))
               (menu-item () (replace-menu-target-command "Maker"
                               (make-primitive-menu-item-content
                                (remove nil
                                        (map-product (lambda (type initform)
                                                       (when (or (consp initform)
                                                                 (eq initform :unbound)
                                                                 (typep initform type))
                                                         (list
                                                          (label () (format nil "type: ~A, initform: ~A " type initform))
                                                          (apply #'make-maker type (unless (eq initform :unbound)
                                                                                     (list :initform initform))))))
                                                     types (append initforms values))))))
               (menu-item () (replace-menu-target-command "Inspector"
                               (make-primitive-menu-item-content
                                (remove nil
                                        (map-product (lambda (type value edited)
                                                       (when (typep value type)
                                                         (bind ((inspector (apply #'make-inspector
                                                                                  type
                                                                                  :edited edited
                                                                                  (unless (eq value :unbound)
                                                                                    (list :component-value value)))))
                                                           (list
                                                            (inline-render-component/widget ()
                                                              (bind ((value (if (slot-boundp inspector 'component-value)
                                                                                (component-value-of inspector)
                                                                                :unbound)))
                                                                <span ,(format nil "type: ~A, value: ~A, edited: ~A " type value edited)>))
                                                            inspector))))
                                                     types values '(#f #t))))))
               (menu-item () (replace-menu-target-command "Filter"
                               (make-primitive-menu-item-content
                                (map-product (lambda (type)
                                               (list
                                                (label () (format nil "type: ~A " type))
                                                (make-place-filter type)))
                                             types)))))))
    (menu-item () "Primitive"
      (make-primitive-menu-item 't '(t) '(:unbound nil #t 42 "alma" 'korte (anything)) nil)
      (make-primitive-menu-item 'boolean '(boolean #+wui-and-cl-perec (or prc::unbound boolean)) '(#f #t) '(:unbound (monday?)))
      (make-primitive-menu-item 'string '(string (or null string)) '(nil "alma") '(:unbound (user-name)))
      (make-primitive-menu-item 'symbol '(symbol (or null symbol)) '(nil eval) '(:unbound (find-symbol "EVAL" "COMMON-LISP")))
      (make-primitive-menu-item 'number '(number (or null number)) '(nil 3 3.14) '(:unbound (life-universe-and-everything)))
      (make-primitive-menu-item 'integer '(integer (or null integer)) '(nil 3) '(:unbound (life-universe-and-everything)))
      (make-primitive-menu-item 'float '(float (or null float)) '(nil 3.14) '(:unbound (pi)))
      #+wui-and-cl-perec
      (make-primitive-menu-item 'prc::date '(prc::date (or null prc::date)) `(nil ,(local-time:parse-datestring "2008-01-01")) '(:unbound (today)))
      #+wui-and-cl-perec
      (make-primitive-menu-item 'prc::time '(prc::time (or null prc::time)) `(nil ,(local-time:parse-timestring "12:30:00Z")) '(:unbound (midnight)))
      #+wui-and-cl-perec
      (make-primitive-menu-item 'prc::timestamp '(prc::timestamp (or null prc::timestamp)) `(nil ,(local-time:parse-timestring "2008-01-01T12:30:00Z")) '(:unbound (now)))
      (make-primitive-menu-item 'member '((member one two three) (or null (member one two three))) '(nil "alma") '(:unbound (one-plus-one)))
      #+wui-and-cl-perec
      (make-primitive-menu-item 'dmm::html-text '(dmm::html-text (or null dmm::html-text)) '(nil "Hello <b>World</b>") '(:unbound (styled-text))))))

(def function make-help-component ()
  (inline-render-component/widget ()
    <div "This is the help page">))

(def function make-about-component ()
  (inline-render-component/widget ()
    <div "This is the about page">))

;;;;;;
;;; ajax counter example (only a proof-of-concept)

(def component counter-component (remote-identity-mixin)
  ((counter 0)))

(def render counter-component
  <span (:id ,(id-of -self-))
    "A counter local to this component: "
    ,(counter-of -self-)
    " "
    ,(bind ((action (make-action
                      (incf (counter-of -self-)))))
       (render-command "increment" action)
       (render-command "increment without ajax" action :ajax #f))>)
|#

(def book wui (:title "WUI")
  (chapter (:title "Introduction")
    (chapter (:title "What is WUI?")
      )
    (chapter (:title "Why not something else?")
      ))
  (chapter (:title "Installation")
    (chapter (:title "Downloading")
      )
    (chapter (:title "Extracting")
      )
    (chapter (:title "Configuration")
      ))
  (chapter (:title "Starting up")
    (chapter (:title "HTTP server")
      )
    (chapter (:title "Application server")
      )
    (chapter (:title "Component server")
      )
    (chapter (:title "Test")
      ))
  (chapter (:title "Quick tutorial")
    (chapter (:title "Hello world")
      )))

(def macro replace-target-demo/widget (content &body forms)
  `(node/widget ()
       (replace-target-place/widget ()
           ,content
         (demo/widget ()
           ,@forms))))

(def function make-demo-frame-component ()
  (make-demo-frame-component-with-content (empty/layout)))

(def function make-demo-frame-component-with-content (initial-content-component)
  (frame/widget (:title "demo"
                 :stylesheet-uris *demo-stylesheet-uris*
                 :script-uris '#.+demo-script-uris+
                 :page-icon #.+demo-page-icon+)
    (top/widget (:menu-bar (menu-bar/widget ()
                             (make-debug-menu-item)))
      (bind ((content (content/widget ()
                        initial-content-component)))
        (target-place/widget (:target-place (make-component-place content))
          (horizontal-list/layout ()
            (tree/widget ()
              (node/widget ()
                  "Component"
                (node/widget (:expanded #f)
                    "Immediate"
                  (replace-target-demo/widget "Number"
                    42)
                  (replace-target-demo/widget "String"
                    "Hello World"))
                (node/widget (:expanded #f)
                    "Layout"
                  (replace-target-demo/widget "Empty"
                    (empty/layout))
                  (replace-target-demo/widget "Alternator"
                    ;; a layout does not have behavior, so switching between alternative is only possible programatically
                    (alternator/layout ()
                      "John"
                      "Mary"
                      "Steve"
                      "Kate"))
                  (replace-target-demo/widget "Vertical List"
                    (vertical-list/layout ()
                      "John"
                      "Mary"
                      "Steve"
                      "Kate"))
                  (replace-target-demo/widget "Horizontal List"
                    (horizontal-list/layout ()
                      "John"
                      "Mary"
                      "Steve"
                      "Kate"))
                  (replace-target-demo/widget "Flow"
                    (flow/layout ()
                      "John "
                      "Mary "
                      "Steve "
                      "Kate "
                      "Fred "
                      "Susanne "
                      "George "
                      "Jenna "))
                  (replace-target-demo/widget "Container"
                    ;; see demo.css
                    (container/layout (:id "container")
                      "John "
                      "Mary "
                      "Steve "
                      "Kate "
                      "Fred "))
                  (replace-target-demo/widget "Table"
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
                  (replace-target-demo/widget "Tree"
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
                  (replace-target-demo/widget "Treeble"
                    (treeble/layout ()
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
                  (replace-target-demo/widget "XY"
                    (xy/layout (:width 200 :height 200)
                      (parent-relative-position/layout (:x 100 :y 100)
                        "John")
                      (parent-relative-position/layout (:x 50 :y 150)
                        "Mary")
                      (parent-relative-position/layout (:x 120 :y 70)
                        "Steve")
                      (parent-relative-position/layout (:x 80 :y 50)
                        "Kate"))))
                (node/widget (:expanded #t)
                    "Widget"
                  (replace-target-demo/widget "Inline render XHTML"
                    (inline-render-xhtml/widget ()
                      <div <span (:style "color: blue") "John">
                           <span (:style "color: red") "Mary">>))
                  (replace-target-demo/widget "Wrap render XHTML"
                    (wrap-render-xhtml/widget ()
                        "The wrapped component is now a simple string"
                      <span ">>> "
                        <span (:style "color: blue")
                          ,(-body-)>
                        " <<<">))
                  (replace-target-demo/widget "Inline XHTML string content"
                    (inline-xhtml-string-content/widget ()
                      "<div><span style=\"color: blue\">John</span><span style=\"color: red\">Mary</span></div>"))
                  (replace-target-demo/widget "Quote XML string content"
                    (quote-xml-string-content/widget ()
                      "<div><span style=\"color: blue\">John</span><span style=\"color: red\">Mary</span></div>"))
                  (replace-target-demo/widget "Quote XML form"
                    (quote-xml-form/widget ()
                      <div <span (:style ,(concatenate-string "color:" " blue")) "John">
                           <span (:style "color: red") "Mary">>))
                  (replace-target-demo/widget "Collapsible"
                    (collapsible/widget ()
                      "SICP"
                      "Structure and Interpretation of Computer Programs"))
                  (replace-target-demo/widget "Alternator"
                    (alternator/widget ()
                      "John"
                      "Mary"
                      "Steve"
                      "Kate"))
                  (replace-target-demo/widget "Tab container"
                    (tab-container ()
                      (tab-page (:selector (icon switch-to-tab-page :label "Male"))
                        "John")
                      (tab-page (:selector (icon switch-to-tab-page :label "Female"))
                        "Mary")))
                  (replace-target-demo/widget "Menu bar"
                    (menu-bar/widget ()
                      (menu-item/widget ()
                          "John"
                        (menu-item/widget ()
                            "Mary")
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
                  (replace-target-demo/widget "Popup menu"
                    (popup-menu/widget ()
                        "Right click for popup menu"
                      (menu-item/widget ()
                          "John"
                        (menu-item/widget ()
                            "Mary")
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
                  (replace-target-demo/widget "Context menu"
                    (content/widget (:context-menu (context-menu/widget ()
                                                     (menu-item/widget ()
                                                         "John"
                                                       (menu-item/widget ()
                                                           "Mary")
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
                  (replace-target-demo/widget "Command"
                    (bind ((c 0))
                      (vertical-list ()
                        (command/widget ()
                          "Click me"
                          (make-action
                            (incf c)))
                        (inline-render-xhtml/widget ()
                          <span "Click counter: " ,c>))))
                  (replace-target-demo/widget "Command bar"
                    (bind ((s nil))
                      (vertical-list ()
                        (command-bar/widget ()
                          (command/widget ()
                            (icon refresh-component)
                            (make-action
                              (setf s "refresh")))
                          (command/widget ()
                            (icon select-component)
                            (make-action
                              (setf s "select"))))
                        (inline-render-xhtml/widget ()
                          <span "Last command: " ,s>))))
                  (replace-target-demo/widget "Push button"
                    (push-button/widget ()
                      (command ()
                        (icon refresh-component)
                        (make-action))))
                  (replace-target-demo/widget "Toggle button"
                    (toggle-button/widget ()
                      (command ()
                        (icon refresh-component)
                        (make-action))))
                  (replace-target-demo/widget "Drop down button"
                    (drop-down-button/widget ()
                      (command ()
                        (icon refresh-component)
                        (make-action))))
                  (replace-target-demo/widget "List"
                    (list/widget ()
                      (element/widget ()
                        "John")
                      (element/widget ()
                        "Mary")
                      (element/widget ()
                        "Steve")
                      (element/widget ()
                        "Kate")))
                  (replace-target-demo/widget "Name value list"
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
                  (replace-target-demo/widget "Table"
                    "TODO"
                    #+nil
                    (table/widget (:columns (list (column/widget ()
                                                                 )
                                                  (column/widget ()
                                                                 )))
                                  (row/widget ()
                                              (cell/widget ()
                                                           "John")
                                              (cell/widget ()
                                                           "Mary"))
                                  (row/widget ()
                                              (cell/widget ()
                                                           "Steve")
                                              (cell/widget ()
                                                           "Kate"))))
                  (replace-target-demo/widget "Tree"
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
                  (replace-target-demo/widget "Treeble"
                    "TODO"
                    #+nil
                    (treeble/widget (:columns (list (column/widget ()
                                                                   )
                                                    (column/widget ()
                                                                   )))
                                    (node/widget ()
                                        (list (cell/widget ()
                                                           "John")
                                              (cell/widget ()
                                                           "Mary"))
                                      (node/widget ()
                                          (list (cell/widget ()
                                                             "Steve")
                                                (cell/widget ()
                                                             "Kate"))))))
                  (replace-target-demo/widget "Tree navigator"
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
                                   :node "Dél-magyarországi régió")))
                (node/widget (:expanded #f)
                    "Chart"
                  (replace-target-demo/widget "Column"
                    "TODO")
                  (replace-target-demo/widget "Flow"
                    "TODO")
                  (replace-target-demo/widget "Line"
                    "TODO")
                  (replace-target-demo/widget "Pie"
                    "TODO")
                  (replace-target-demo/widget "Radar"
                    "TODO")
                  (replace-target-demo/widget "Scatter"
                    "TODO")
                  (replace-target-demo/widget "Stock"
                    "TODO")
                  (replace-target-demo/widget "Structure"
                    "TODO"))
                (node/widget (:expanded #f)
                    "Book"
                  (replace-target-demo/widget "Book"
                    "TODO")
                  (replace-target-demo/widget "Chapter"
                    "TODO")
                  (replace-target-demo/widget "Glossary"
                    "TODO")
                  (replace-target-demo/widget "Paragraph"
                    "TODO")
                  (replace-target-demo/widget "Toc"
                    "TODO"))
                (node/widget (:expanded #f)
                    "Model"
                  (replace-target-demo/widget "System"
                    "TODO")
                  (replace-target-demo/widget "Module"
                    "TODO")
                  (replace-target-demo/widget "File"
                    "TODO")
                  (replace-target-demo/widget "Package"
                    "TODO")
                  (replace-target-demo/widget "Dictionary"
                    "TODO")
                  (replace-target-demo/widget "Name"
                    "TODO")
                  (replace-target-demo/widget "Variable"
                    "TODO")
                  (replace-target-demo/widget "Type"
                    "TODO")
                  (replace-target-demo/widget "Class"
                    "TODO")
                  (replace-target-demo/widget "Slot"
                    "TODO")
                  (replace-target-demo/widget "Form"
                    "TODO")
                  (replace-target-demo/widget "Function"
                    "TODO")
                  (replace-target-demo/widget "Generic function"
                    "TODO")
                  (replace-target-demo/widget "Generic method"
                    "TODO"))
                (node/widget (:expanded #f)
                    "Meta"
                  (node/widget (:expanded #f)
                      "Primitive"
                    (replace-target-demo/widget "String"
                      "TODO")
                    (replace-target-demo/widget "Member"
                      "TODO")
                    (replace-target-demo/widget "Integer"
                      "TODO")
                    (replace-target-demo/widget "Float"
                      "TODO"))
                  (node/widget (:expanded #f)
                      "Place"
                    (replace-target-demo/widget "Lexical"
                      "TODO")
                    (replace-target-demo/widget "Special"
                      "TODO")
                    (replace-target-demo/widget "Slot"
                      "TODO"))
                  (node/widget (:expanded #f)
                      "Object"
                    (replace-target-demo/widget "Object"
                      "TODO")
                    (replace-target-demo/widget "Object list"
                      "TODO")
                    (replace-target-demo/widget "Object tree"
                      "TODO")
                    ;; TODO: move these?
                    (replace-target-demo/widget "Lisp form invoker"
                      (vertical-list ()
                        (lisp-form/invoker ()
                          ｢(def function dwim (text &rest args &key (baz 0) &allow-other-keys)
                             (let* ((foo (sqrt baz))
                                    (bar (1+ foo)))
                               (if (string= text "Hello World")
                                   (length args)
                                   (+ foo bar baz))))｣)
                        (lisp-form/invoker (:evaluation-mode :multiple)
                          (print "Hello World"))))
                    (replace-target-demo/widget "Standard class tree viewer"
                      (standard-class/tree/viewer ()
                        (find-class 'component)))
                    (replace-target-demo/widget "Standard class tree level viewer"
                      (standard-class/tree-level/viewer ()
                        (find-class 'tree-level/widget)))
                    (replace-target-demo/widget "Book tree level viewer"
                      (book/tree-level/viewer ()
                        (find-book 'wui)))))
                ;; TODO: delete this stuff
                (node/widget (:expanded #f)
                    "RANDOM"
                  (replace-target-demo/widget "Lisp form list repl inspector"
                    (lisp-form-list/repl/inspector ())))))
            content))))))

#|
;; TODO: this must not be a widget
(replace-target-demo/widget "External link"
  (external-link/widget ()
    "http://wikipedia.org"
    "Wikipedia"))

(node/widget ()
    "Maker")
(node/widget ()
    "Viewer")
(node/widget ()
    "Editor")
(node/widget ()
    "Inspector")
(node/widget ()
    "Filter")
(node/widget ()
    "Finder")
(node/widget ()
    "Selector")
(node/widget ()
    "Invoker")
|#

;;;;;;
;;; the entry points

(def file-serving-entry-point *demo-application* "/static/" (system-relative-pathname :hu.dwim.wui "wwwroot/"))

(def js-file-serving-entry-point *demo-application* "/wui/js/" (system-relative-pathname :hu.dwim.wui "src/js/"))

(def entry-point (*demo-application* :path "" :ensure-session #t :ensure-frame #t) ()
  (assert (and (boundp '*session*) *session*))
  (assert (and (boundp '*frame*) *frame*))
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*) (make-demo-frame-component))
        (make-redirect-response-for-current-application))))

(def entry-point (*demo-application* :path "session-info" :priority 1000) ()
  (if *session*
      (progn
        (setf (root-component-of *frame*) (make-value-viewer *session*))
        (if (wui::find-frame-for-request *session*)
            (make-root-component-rendering-response *frame*)
            (make-redirect-response-for-current-application "session-info")))
      (make-functional-html-response ()
        <p "There's no session">)))

(def entry-point (*demo-application* :path "about/") ()
  (make-component-rendering-response (make-demo-frame-component)))

(def entry-point (*demo-application* :path "help/") ()
  (make-component-rendering-response (make-demo-frame-component)))

#|
(def entry-point (*demo-application* :path +login-entry-point-path+ :with-session-logic #f)
    (identifier password continue-url user-action)
  (%login-entry-point identifier password continue-url user-action))

(def function %login-entry-point (identifier password continue-url user-action?)
  (when (and continue-url
             (zerop (length (string-trim " " continue-url))))
    (setf continue-url nil))
  (when (or (null identifier)
            (zerop (length identifier)))
    (setf identifier (cookie-value +login-identifier-cookie-name+)))
  (when identifier
    (setf identifier (string-trim " " identifier))
    (when (zerop (length identifier))
      (setf identifier nil)))
  (when password
    (setf password (string-trim " " password))
    (when (zerop (length password))
      (setf password nil)))
  ;; ok, params are all ready for being processed
  (bind ((application *application*)
         (authenticated-subject nil))
    (with-session-logic ()
      (when *session*
        ;;(authentication.dribble "Login entry point reached while we already have a session: ~A" *session*)
        (return-from %login-entry-point
          (if continue-url
              (make-redirect-response continue-url)
              (make-redirect-response-for-current-application)))))
    ;; TODO: there's no user feedback if not valid!
    (when (and (valid-login-identifier? identifier)
               (valid-login-password? password))
      (when (string= password "secret")
        (setf authenticated-subject identifier)))
    (if authenticated-subject
        (bind ((new-session (make-new-session application)))
          (setf *session* new-session)
          ;;(audit.info "Succesfull authentication ~A with ~A by ~A" authenticated-session (authentication-instrument-of authenticated-session) (authenticated-subject-of authenticated-session))
          (setf (authenticated-subject-of new-session) authenticated-subject)
          (with-lock-held-on-application (application)
            (register-session application new-session))
          (bind ((response (if continue-url
                               (make-redirect-response continue-url)
                               (make-redirect-response-for-current-application))))
            (add-cookie (make-cookie +login-identifier-cookie-name+ identifier
                                     :max-age #.(* 60 60 24 365 100)
                                     :domain (concatenate-string "." (host-of (uri-of *request*)))
                                     :path (concatenate-string (path-prefix-of application) +login-entry-point-path+))
                        response)
            (decorate-application-response application response)
            response))
        (bind (((:values frame main-component) (make-demo-frame-component))
               (login-component (find-descendant-component-with-type main-component 'identifier-and-password-login-component)))
          (assert login-component)
          (setf (identifier-of login-component) identifier)
          (setf (password-of login-component) password)
          (when (or password
                    user-action?)
            (add-user-error login-component "Login failed"))
          (make-component-rendering-response frame)))))

(def method handle-request-to-invalid-session ((application demo-application) session invalidity-reason)
  (bind ((uri (make-uri-for-current-application +login-entry-point-path+)))
    (setf (uri-query-parameter-value uri +continue-url-query-parameter-name+)
          (print-uri-to-string (clone-request-uri :strip-query-parameters wui::+frame-query-parameter-names+)))
    (when (eq invalidity-reason :timed-out)
      (setf (uri-query-parameter-value uri +session-timed-out-query-parameter-name+) "t"))
    (make-redirect-response uri)))

(def function valid-login-password? (password)
  (and password
       (>= (length password) +minimum-login-password-length+)))

(def function valid-login-identifier? (identifier)
  (and identifier
       (>= (length identifier) +minimum-login-identifier-length+)))
|#

(def function start-test-server-with-demo-application (&key (maximum-worker-count 16) (log-level +debug+) (host *test-host*) (port *test-port*))
  (setf (log-level 'wui) log-level)
  (start-test-server-with-brokers (list *demo-application*
                                        (make-redirect-broker "" "/"))
                                  :host host
                                  :port port
                                  :maximum-worker-count maximum-worker-count
                                  :request-content-length-limit (* 1024 1024 50)))

#|

;;;;;;
;;; Test classes

(def class* child-test ()
  ((name :type string)
   (size :type (member :small :big))
   (parent :type parent-test)))

(def class* parent-test ()
  ((important #f :type boolean)
   (age nil :type (or null integer))
   (name nil :type (or null string))))

(def resources en
  (class-name.child-test "child")
  (class-name.parent-test "parent")
  (slot-name.name "name")
  (slot-name.size "size")
  (slot-name.important "important")
  (slot-name.age "age")
  (child-test.parent "parent"))

(def resources hu
  (class-name.child-test "gyerek")
  (class-name.parent-test "szülő")
  (slot-name.name "név")
  (slot-name.size "size")
  (slot-name.important "fontos")
  (slot-name.age "kor")
  (child-test.parent "szülő"))

;;;;;;
;;; Test instances

(def special-variable *test-instances* nil)

(def function make-test-instances ()
  (setf *test-instances*
        (append *test-instances*
                (bind ((parent-1 (make-instance 'parent-test :name "Parent/1" :important #t))
                       (parent-2 (make-instance 'parent-test)))
                  (list parent-1
                        (make-instance 'child-test :name "Child/1" :parent parent-1 :size :small)
                        (make-instance 'child-test :name "Child/2" :parent parent-2 :size :small)
                        (make-instance 'child-test :name "Child/3" :parent parent-2 :size :big))))))

(make-test-instances)

|#