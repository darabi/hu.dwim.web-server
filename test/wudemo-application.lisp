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
(def (constant :test 'string=) +login-identifier-cookie-name+ "login-identifier")

(def class* wudemo-application (application-with-home-package)
  ()
  (:metaclass funcallable-standard-class))

(def class* wudemo-session ()
  ((authenticated-subject nil)))

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
   (parent :type parent-test)))

(def class* parent-test ()
  ((name :type string)))

;;;;;;
;;; Test instances

(def special-variable *test-instances* nil)

(def function make-test-instances ()
  (setf *test-instances*
        (append *test-instances*
                (bind ((parent-1 (make-instance 'parent-test :name "Parent/1")))
                  (list parent-1
                        (make-instance 'child-test :name "Child/1" :parent parent-1)
                        (make-instance 'child-test :name "Child/2" :parent parent-1))))))

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
                                                                         (mark-expired *session*)
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
      (bind ((path (path-of (uri-of *request*))))
        (assert (starts-with-subseq (path-prefix-of *application*) path))
        (setf path (subseq path (length (path-prefix-of *application*))))
        (switch (path :test #'string=)
          ("login/" (vertical-list ()
                      (make-identifier-and-password-login-component)
                      (inline-component <div "FYI, the password is \"secret\"...">)))
          ("help/" (make-help-component))
          ("about/" (make-about-component))
          (t (inline-component <span "wudemo start page">))))))

(def function make-unauthenticated-menu-component ()
  (menu nil
    (menu-item (command (label #"menu.front-page") (make-application-relative-uri "")))
    (menu-item (command (label #"menu.help") (make-application-relative-uri "help/")))
    (menu-item (command (label #"menu.login") (make-application-relative-uri "login/")))
    (menu-item (command (label #"menu.about") (make-application-relative-uri "about/")))))

(def function make-authenticated-menu-component ()
  (bind ((authenticated-subject (current-authenticated-subject))
         (god? (string= "god" authenticated-subject)))
    (menu nil
      (when god?
        (make-debug-menu))
      (menu "Child"
        (menu-item (replace-menu-target-command "Make a child" (make-maker 'child-test)))
        (menu-item (replace-menu-target-command "Search children" (make-filter 'child-test))))
      (menu "Parent"
        (menu-item (replace-menu-target-command "Make a parent" (make-maker 'parent-test)))
        (menu-item (replace-menu-target-command "Search parents" (make-filter 'parent-test))))
      (menu "Others"
        (menu-item (replace-menu-target-command (label #"menu.help") (make-help-component)))
        (menu-item (replace-menu-target-command (label #"menu.about") (make-about-component)))))))

(def function make-help-component ()
  (inline-component <div "This is the help page">))

(def function make-about-component ()
  (inline-component <div "This is the about page">))

;;;;;;
;;; the entry points

(def file-serving-entry-point *wudemo-application* "/static/" (system-relative-pathname :wui "wwwroot/"))

(def js-file-serving-entry-point *wudemo-application* "/wui/js/" (system-relative-pathname :wui "src/js/"))

(def entry-point (*wudemo-application* :path "") ()
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

(def entry-point (*wudemo-application* :path "login/" :lookup-and-lock-session #f) (identifier password continue-uri user-action)
  (%login-entry-point identifier password continue-uri user-action))

(def function %login-entry-point (identifier password continue-uri user-action?)
  (when (and continue-uri
             (zerop (length (string-trim " " continue-uri))))
    (setf continue-uri nil))
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
    (with-session/frame/action-logic ()
      (when *session*
        ;;(authentication.dribble "Login entry point reached while we already have a session: ~A" *session*)
        (return-from %login-entry-point
          (if continue-uri
              (make-redirect-response continue-uri)
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
          (bind ((response (if continue-uri
                               (make-redirect-response continue-uri)
                               (make-redirect-response-for-current-application))))
            (add-cookie (make-cookie +login-identifier-cookie-name+ identifier
                                     :max-age #.(* 60 60 24 365 100)
                                     :domain (concatenate-string "." (host-of (uri-of *request*)))
                                     :path (concatenate-string (path-prefix-of application) "login/"))
                        response)
            (decorate-application-response application response)
            response))
        (bind (((:values frame main-component) (make-wudemo-frame-component))
               (login-component (find-descendant-component-with-type main-component 'identifier-and-password-login-component)))
          (setf (identifier-of login-component) identifier)
          (setf (password-of login-component) password)
          (when (or password
                    user-action?)
            (add-user-error login-component "Login failed"))
          (make-component-rendering-response frame)))))

(def function valid-login-password? (password)
  (and password
       (>= (length password) +minimum-login-password-length+)))

(def function valid-login-identifier? (identifier)
  (and identifier
       (>= (length identifier) +minimum-login-identifier-length+)))

(def function start-server-with-wudemo-application (&key (maximum-worker-count 16) (log-level +debug+))
  (with-logger-level wui log-level
    (start-server-with-brokers (list *wudemo-application*
                                     (make-redirect-broker "" "/"))
                               :maximum-worker-count maximum-worker-count)))
