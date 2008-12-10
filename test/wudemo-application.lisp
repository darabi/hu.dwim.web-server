(in-package :wui-test)

;;;;;;
;;; this is a simple example application with login and logout support

(def (constant :test 'equalp) +wudemo-stylesheet-uris+ '("static/css/wudemo.css"
                                                         "static/wui/css/wui.css"
                                                         "static/wui/css/wui-default-skin.css"
                                                         "static/dojo/dijit/themes/tundra/tundra.css"
                                                         "static/dojo/dojo/resources/dojo.css"))

(def (constant :test 'equalp) +wudemo-script-uris+ '("wui/js/wui.js"))

(def (constant :test 'string=) +wudemo-page-icon+ "static/favicon.ico")

(def constant +minimum-login-password-length+ 6)
(def constant +minimum-login-identifier-length+ 3)

(def class* wudemo-application (application-with-home-package)
  ()
  (:metaclass funcallable-standard-class))

(def class* wudemo-session ()
  ((authenticated-subject nil)
   (example-inline-edit-box-value "click to edit this...")))

(def function current-authenticated-subject ()
  (and *session*
       (authenticated-subject-of *session*)))

(def function is-authenticated? ()
  (not (null (current-authenticated-subject))))

(def method session-class list ((application wudemo-application))
  'wudemo-session)

(def special-variable *wudemo-application*
  (make-instance 'wudemo-application
                 :path-prefix "/"
                 :home-package (find-package :wui-test)
                 :default-locale "hu"))

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

;;;;;;
;;; the factories that create the component graph both in logged out and logged in states

(def function make-wudemo-frame-component ()
  (make-wudemo-frame-component-with-content (make-wudemo-menu-content-component)))

(def function make-wudemo-frame-component-with-content (menu-content)
  (bind ((menu (make-wudemo-menu-component)))
    (bind ((frame-component (frame (:title "wudemo"
                                           :stylesheet-uris '#.+wudemo-stylesheet-uris+
                                           :script-uris '#.+wudemo-script-uris+
                                           :page-icon #.+wudemo-page-icon+)
                              (vertical-list (:id "page")
                                (when *session*
                                  (style (:id "header" :css-class "authenticated")
                                    (inline-component <span ,(current-authenticated-subject)
                                                            " "
                                                            ,(when (running-in-test-mode-p *wudemo-application*)
                                                                   "(test mode)")
                                                            " "
                                                            ,(when (profile-request-processing-p *server*)
                                                                   "(profiling)")
                                                            " "
                                                            ,(when (debug-client-side? (root-component-of *frame*))
                                                                   "(debugging client side)")>
                                                      <span ,(render
                                                              (command (icon logout)
                                                                       (make-action
                                                                         (mark-session-invalid *session*)
                                                                         (make-redirect-response-for-current-application))))>)))
                                (horizontal-list ()
                                  (style (:id "menu") menu)
                                  (top menu-content))))))
      (setf (target-place-of menu) (make-component-place menu-content))
      (values frame-component menu-content))))

(def method make-frame-component-with-content ((application wudemo-application) content)
  (make-wudemo-frame-component-with-content content))

(def function make-wudemo-menu-component ()
  (if (is-authenticated?)
      (make-authenticated-menu-component)
      (make-unauthenticated-menu-component)))

(def function make-wudemo-menu-content-component ()
  (if (is-authenticated?)
      (empty)
      (if (parameter-value "example-error")
          (inline-component
            (error "This is an example error that happens while rendering the response"))
          (bind ((path (path-of (uri-of *request*))))
            (assert (starts-with-subseq (path-prefix-of *application*) path))
            (setf path (subseq path (length (path-prefix-of *application*))))
            (switch (path :test #'string=)
              (+login-entry-point-path+ (vertical-list ()
                                          (make-identifier-and-password-login-component)
                                          (inline-component <div "FYI, the password is \"secret\"...">)))
              ("help/" (make-help-component))
              ("about/" (make-about-component))
              (t (inline-component <span "wudemo start page">)))))))

(def function make-unauthenticated-menu-component ()
  (macrolet ((command* (title path)
               `(command ,title
                         ,(if (stringp path)
                              `(make-application-relative-uri ,path)
                              path)
                         :send-client-state #f)))
    (menu nil
      (menu-item () (command* #"menu.front-page"              ""))
      (menu-item () (command* #"menu.help"                    "help/"))
      (menu-item () (command* #"menu.login"                   +login-entry-point-path+))
      (menu-item () (command* #"menu.about"                   "about/"))
      (menu-item () (command* #"menu.example-rendering-error" (bind ((uri (make-application-relative-uri "")))
                                                                (setf (uri-query-parameter-value uri "example-error") "t")
                                                                uri))))))

;; TODO the onChange part hould be this simple, but it needs qq work.
;; ,(js-to-lisp-rpc
;;   (setf (example-inline-edit-box-value-of *session*) (aref |arguments| 0)))

(def function render-example-inline-edit-box ()
  `js(dojo.require "dijit.InlineEditBox")
  (bind ((id (generate-frame-unique-string "inlineEditor")))
    <span
      "This is stored in the session, click to edit: "
      ,(render-dojo-widget (id)
         <span (:id ,id
                :dojoType "dijit.InlineEditBox"
                :autoSave false
                :onChange
                `js-inline*(wui.io.xhr-post
                            (create
                             :content (create :example-inline-edit-box-value (aref arguments 0))
                             :url ,(action/href (:delayed-content #t)
                                     (with-request-params (((value "exampleInlineEditBoxValue") "this is the default value; it's a bug!"))
                                       (setf (example-inline-edit-box-value-of *session*) value))
                                     ;; TODO this is, well, proof-of-concept quality for now...
                                     (make-do-nothing-response))
                             :load (lambda (response args)))))
           ,(example-inline-edit-box-value-of *session*)>)>))

(def function make-authenticated-menu-component ()
  (bind ((authenticated-subject (current-authenticated-subject)))
    (menu nil
      (when (> (length authenticated-subject) 0) ; just a random condition for demo purposes
        (bind ((debug-menu (make-debug-menu)))
          (appendf (menu-items-of debug-menu)
                   (list (menu-item () (command "Example error in action body"
                                             (make-action (error "This is an example error which is signaled when running the action body"))))
                         (menu-item () (replace-menu-target-command "Example error while rendering"
                                         (inline-component (error "This is an example error which is signaled when rendering the root component of the current frame"))))))
          debug-menu))
      (menu "Charts"
       (menu "Charts from files"
         (macrolet ((make-chart (type settings-file data-file)
                      `(make-chart-from-files ',type
                                              :settings-file (project-relative-pathname ,(concatenate-string "test/amCharts/examples/" settings-file))
                                              :data-file (project-relative-pathname ,(concatenate-string "test/amCharts/examples/" data-file)))))
           (menu-item () (replace-menu-target-command "Column chart"
                           (make-chart column-chart
                                       "amcolumn/3d_stacked_bar_chart/amcolumn_settings.xml"
                                       "amcolumn/3d_stacked_bar_chart/amcolumn_data.txt")))
          (menu-item () (replace-menu-target-command "Line chart"
                          (make-chart line-chart
                                      "amline/stacked_area_chart/amline_settings.xml"
                                      "amline/stacked_area_chart/amline_data.xml")))
          (menu-item () (replace-menu-target-command "Pie chart"
                          (make-chart pie-chart
                                      "ampie/donut/ampie_settings.xml"
                                      "ampie/donut/ampie_data.txt")))
          (menu-item () (replace-menu-target-command "Radar chart"
                          (make-chart radar-chart
                                      "amradar/stacked/amradar_settings.xml"
                                      "amradar/stacked/amradar_data.xml")))
          (menu-item () (replace-menu-target-command "Stock chart"
                          (make-chart stock-chart
                                      "amstock/ohlc/amstock_settings.xml"
                                      "amstock/ohlc/data.csv")))
          (menu-item () (replace-menu-target-command "Xy chart"
                          (make-chart xy-chart
                                      "amxy/time_plot/amxy_settings.xml"
                                      "amxy/time_plot/amxy_data.xml"))))))
      (make-primitive-component-menu)
      (menu "Metagui"
        (menu "Parent"
          (menu-item () (replace-menu-target-command "Make a parent" (make-maker 'parent-test)))
          (menu-item () (replace-menu-target-command "Search parents" (make-filter 'parent-test))))
        (menu "Child"
          (menu-item () (replace-menu-target-command "Make a child" (make-maker 'child-test)))
          (menu-item () (replace-menu-target-command "Search children" (make-filter 'child-test)))))
      (menu-item () (replace-menu-target-command "Dojo InlineEditBox example"
                      (inline-component
                        (render-example-inline-edit-box))))
      (menu-item () (replace-menu-target-command "checkbox"
                      (bind ((value1 #t)
                             (value2 #f))
                        (vertical-list ()
                          (inline-component
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
      (menu "Others"
        (menu-item () (replace-menu-target-command #"menu.help" (make-help-component)))
        (menu-item () (replace-menu-target-command #"menu.about" (make-about-component)))))))

(def function make-primitive-component-menu ()
  (labels ((make-primitive-menu-item-content (components)
             (inline-component
               <div ,(render (command (icon wui::refresh)
                                      (make-action)))
                    <table ,(foreach (lambda (component)
                                       <tr ,(foreach (lambda (cell)
                                                       <td ,(render cell)>)
                                                     component)>)
                                     components)>>))
           (make-primitive-menu-item (name types values initforms)
             (menu (string-capitalize (string-downcase (symbol-name name)))
               (menu-item () (replace-menu-target-command "Maker"
                               (make-primitive-menu-item-content
                                (remove nil
                                        (map-product (lambda (type initform)
                                                       (when (or (consp initform)
                                                                 (eq initform :unbound)
                                                                 (typep initform type))
                                                         (list
                                                          (label (format nil "type: ~A, initform: ~A " type initform))
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
                                                            (inline-component
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
                                                (label (format nil "type: ~A " type))
                                                (make-place-filter type)))
                                             types)))))))
    (menu "Primitive"
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
  (inline-component <div "This is the help page">))

(def function make-about-component ()
  (inline-component <div "This is the about page">))

;;;;;;
;;; ajax counter example (only a proof-of-concept)

(def component counter-component (remote-identity-component-mixin)
  ((counter 0)))

(def render counter-component
  <span (:id ,(id-of -self-))
    "A counter local to this component: "
    ,(counter-of -self-)
    " "
    ,(bind ((action-href (escape-as-xml ; FIXME this should be handled by cl-qq
                          (action/href ()
                            (incf (counter-of -self-))))))
       <a (:href `js-inline(wui.io.action ,action-href))
          "increment">
       <a (:href `js-inline(wui.io.action ,action-href false))
          "increment without ajax">) >)

;;;;;;
;;; the entry points

(def file-serving-entry-point *wudemo-application* "/static/" (system-relative-pathname :wui "wwwroot/"))

(def js-file-serving-entry-point *wudemo-application* "/wui/js/" (system-relative-pathname :wui "src/js/"))

(def entry-point (*wudemo-application* :path "" :ensure-session #f :ensure-frame #t) ()
  (if *session*
      (progn
        (assert (and (boundp '*frame*) *frame*))
        (if (root-component-of *frame*)
            (make-root-component-rendering-response *frame*)
            (progn
              (setf (root-component-of *frame*) (make-wudemo-frame-component))
              (make-redirect-response-for-current-application))))
      (make-component-rendering-response (make-wudemo-frame-component))))

(def entry-point (*wudemo-application* :path "about/") ()
  (make-component-rendering-response (make-wudemo-frame-component)))

(def entry-point (*wudemo-application* :path "help/") ()
  (make-component-rendering-response (make-wudemo-frame-component)))

(def entry-point (*wudemo-application* :path +login-entry-point-path+ :with-session-logic #f)
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
        (bind (((:values frame main-component) (make-wudemo-frame-component))
               (login-component (find-descendant-component-with-type main-component 'identifier-and-password-login-component)))
          (assert login-component)
          (setf (identifier-of login-component) identifier)
          (setf (password-of login-component) password)
          (when (or password
                    user-action?)
            (add-user-error login-component "Login failed"))
          (make-component-rendering-response frame)))))

(def method handle-request-to-invalid-session ((application wudemo-application) session invalidity-reason)
  (bind ((uri (make-uri-for-current-application +login-entry-point-path+)))
    (setf (uri-query-parameter-value uri +continue-url-query-parameter-name+)
          (print-uri-to-string (clone-request-uri :strip-frame-parameters #t)))
    (when (eq invalidity-reason :timed-out)
      (setf (uri-query-parameter-value uri +session-timed-out-query-parameter-name+) "t"))
    (make-redirect-response uri)))

(def function valid-login-password? (password)
  (and password
       (>= (length password) +minimum-login-password-length+)))

(def function valid-login-identifier? (identifier)
  (and identifier
       (>= (length identifier) +minimum-login-identifier-length+)))

(def function start-test-server-with-wudemo-application (&key (maximum-worker-count 16) (log-level +debug+) (host *test-host*) (port *test-port*))
  (setf (log-level 'wui) log-level)
  (start-test-server-with-brokers (list *wudemo-application*
                                        (make-redirect-broker "" "/"))
                                  :host host
                                  :port port
                                  :maximum-worker-count maximum-worker-count))
