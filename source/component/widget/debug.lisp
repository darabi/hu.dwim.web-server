;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Debug menu

(def (function e) toggle-profile-request-processing/server (&optional (server *server*))
  (notf (profile-request-processing? server)))

(def (function e) toggle-running-in-test-mode/application (&optional (application *application*))
  (notf (running-in-test-mode? application)))

(def (function e) toggle-debug-server-side/application (&optional (application *application*))
  (if (slot-boundp application 'debug-on-error)
      (notf (slot-value application 'debug-on-error))
      (setf (slot-value application 'debug-on-error) (not *debug-on-error*))))

(def (function e) toggle-ajax-enabled/application (&optional (application *application*))
  (notf (ajax-enabled? application)))

(def (function e) toggle-debug-component-hierarchy/frame (&optional (frame *frame*))
  (notf (debug-component-hierarchy? frame)))

(def (function e) toggle-debug-client-side/frame (&optional (frame *frame*))
  (notf (debug-client-side? (root-component-of frame))))

(def (function e) clear-root-component (&optional (frame *frame*))
  (setf (root-component-of frame) nil))

(def (function e) make-debug-menu ()
  (when (authorize-operation *application* '(make-debug-menu))
    (menu-item/widget ()
        "Debug"
      (menu-item/widget ()
          (command/widget (:js (lambda () `js(wui.reload-css)))
            "Reload CSS"))
      (menu-item/widget ()
          "Invalidate"
        (menu-item/widget ()
            (command/widget (:ajax #f :send-client-state #f)
              "Invalidate session"
              (make-action (mark-session-invalid))))
        (menu-item/widget ()
            (command/widget (:ajax #f :send-client-state #f)
              "Invalidate frame"
              (make-action (mark-frame-invalid))))
        (menu-item/widget ()
            (command/widget (:ajax #f :send-client-state #f)
              "Clear the frame's root component"
              (make-action (clear-root-component)))))
      (menu-item/widget ()
          "Inspect"
        (menu-item/widget ()
            (replace-target-place/widget ()
                "Server"
              (make-value-inspector *server*)))
        (menu-item/widget ()
            (replace-target-place/widget ()
                "Application"
              (make-value-inspector *application*)))
        (menu-item/widget ()
            (replace-target-place/widget ()
                "Session"
              (make-value-inspector *session*)))
        (menu-item/widget ()
            (replace-target-place/widget ()
                "Frame"
              (make-value-inspector *frame*)))
        (menu-item/widget ()
            (replace-target-place/widget ()
                "Request"
              (make-value-inspector *request*)))
        (menu-item/widget ()
            (replace-target-place/widget ()
                "Response"
              (make-value-inspector *response*)))
        #+sbcl
        (menu-item/widget ()
            (replace-target-place/widget ()
                "Frame size breakdown"
              (make-instance 'frame-size-breakdown/widget)))
        (menu-item/widget ()
            (replace-target-place/widget ()
                "User agent breakdown"
              (make-value-inspector (make-http-user-agent-breakdown *server*)))))
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Debugging state"
            (vertical-list/layout ()
              (make-instance 'debugging-state/widget))))
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Error handling tests"
            (vertical-list/layout ()
              (make-instance 'debugging-state/widget)
              (make-instance 'error-handling-test/widget)))))))

;;;;;;
;;; Debug menu

(def (component e) debugging-state/widget (standard/widget)
  ())

(def render-xhtml debugging-state/widget
  (with-render-style/component (-self-)
    (labels ((command (action content &rest args)
               (apply 'render-command/xhtml action content :style-class "command" args))
             (replace-target-place-command (component replacement-component content &rest args)
               (apply 'render-replace-target-place-command/xhtml component replacement-component content :style-class "command" args))
             (boolean-to-status-string (value &optional inherited-value)
               (if (eq value :inherited)
                   (string+ (boolean-to-status-string inherited-value) " (inherited)")
                   (if value
                       "enabled"
                       "disabled"))))
      (macrolet ((render-inherited-boolean-status (scope object slot-name default-value &key (ajax #t))
                   (once-only (object slot-name)
                     `<tr <td ,,scope>
                          <td ,(boolean-to-status-string (if (slot-boundp ,object ,slot-name)
                                                             (slot-value ,object ,slot-name)
                                                             :inherited)
                                                         ,default-value)>
                          <td ,(command (make-action
                                          (if (slot-boundp ,object ,slot-name)
                                              (notf (slot-value ,object ,slot-name))
                                              (setf (slot-value ,object ,slot-name) (not ,default-value)))
                                          (mark-to-be-rendered-component -self-))
                                        "toggle" :ajax ,ajax)
                              ", "
                              ,(command (make-action
                                          (slot-makunbound ,object ,slot-name)
                                          (mark-to-be-rendered-component -self-))
                                        "inherit" :ajax ,ajax)>>))
                 (render-global-boolean-status (scope variable-name &key (ajax #t))
                   (once-only (variable-name)
                     `<tr <td ,,scope>
                          <td ,(boolean-to-status-string (symbol-global-value ,variable-name))>
                          <td ,(command (make-action
                                          (notf (symbol-global-value ,variable-name))
                                          (mark-to-be-rendered-component -self-))
                                        "toggle" :ajax ,ajax)>>)))
        <table (:class "bordered")
          <tr <td (:class "header") "Scope of effect" >
              <td (:class "header") "Value" >
              <td (:class "header") "Action" >>
          <tr <td (:class "header" :colspan 3)
                  "Debugging the client side">>
          ,(render-global-boolean-status "Lisp VM" '*debug-client-side* :ajax #f)
          ,(render-inherited-boolean-status "application"
                                            *application* 'debug-client-side
                                            (debug-client-side? *application*) :ajax #f)
          ,(render-inherited-boolean-status "frame/widget"
                                            (root-component-of *frame*) 'debug-client-side
                                            (debug-client-side? (root-component-of *frame*)) :ajax #f)
          <tr <td (:class "header" :colspan 3)
                  "Debugging the server side">>
          ,(render-global-boolean-status "Lisp VM" '*debug-on-error*)
          ,(render-inherited-boolean-status "server" *server* 'debug-on-error
                                            (debug-on-error? *server* nil))
          ,(render-inherited-boolean-status "application" *application* 'debug-on-error
                                            (debug-on-error? *application* nil))
          ,(render-inherited-boolean-status "session" *session* 'debug-on-error
                                            (debug-on-error? *session* nil))
          <tr <td (:class "header" :colspan 3)
                  "Profiling the server request processing">>
          ,(render-global-boolean-status "Lisp VM" '*profile-request-processing*)
          ,(render-inherited-boolean-status "server" *server* 'profile-request-processing?
                                            (profile-request-processing? *server*))>))))

(def component dummy-component-with-environment-restoring-error (standard/widget)
  ())

(def component-environment dummy-component-with-environment-restoring-error
  (error "This error comes from ~S's ~S" 'dummy-component-with-environment-restoring-error 'call-in-component-environment))

(def (component e) error-handling-test/widget (standard/widget)
  ())

(def render-xhtml error-handling-test/widget
  (with-render-style/component (-self-)
    (bind ((error-action (make-action
                           (error "This is a demo error for testing purposes. It was signalled from the body of an action.")))
           (slow-js-logging-ajax-action (make-action
                                          (sleep 2)
                                          (make-functional-response/ajax-aware-client ()
                                            <script `js-inline(log.info "After some delay on the server, we were rendered back...")>))))
     (flet
         ((command (action content &rest args)
            (apply 'render-command/xhtml action content :style-class "command" args))
          (replace-target-place-command (component replacement-component content &rest args)
            (apply 'render-replace-target-place-command/xhtml component replacement-component content :style-class "command" args))
          (make-inline-component-with-rendering-error ()
            (bind ((us nil))
              (setf us (inline-render/widget ()
                                             (error "This is a demo error signaled from the render method of the inline component ~A" us))))))
       <ul
        <li <b "JavaScript errors">>
        <li ,(command nil "OnClick calls undefined function" :js (lambda () `js(this-function-is-undefined)))>
        <li ,(command nil "OnClick throws 'foo'" :js (lambda () `js(throw "foo")))>
        <li ,(command (make-action
                        (make-functional-response/ajax-aware-client ()
                          <script `js-inline(this-function-is-undefined)>))
                      "JavaScript code returned in an AJAX answer calls an undefined function"
                      :ajax #t)>
        <li <b "Errors in actions">>
        <li ,(command error-action "Action time error in a full page reload" :ajax #f)>
        <li ,(command error-action "Action time error in an AJAX request" :ajax #t)>
        <li ,(command (make-action
                        (make-raw-functional-response ()
                          (emit-http-response ((+header/status+       +http-ok+
                                                +header/content-type+ +xml-mime-type+))
                            ;; nothing here intentionally
                            +void+)))
                      "AJAX request receives only the HTTP headers and no body at all" :ajax #t)>
        <li <b "Errors in render methods">>
        <li ,(replace-target-place-command -self- (make-inline-component-with-rendering-error)
                                           "Render time error in a full page reload"
                                           :ajax #f)>
        <li ,(replace-target-place-command -self- (make-inline-component-with-rendering-error)
                                           "Render time error in an AJAX request"
                                           :ajax #T)>
        <li <b "Errors while restoring the component environment">>
        <li ,(replace-target-place-command -self- (make-instance 'dummy-component-with-environment-restoring-error)
                                           "Display a component which errors while restoring its environment; in a full page reload"
                                           :ajax #f)>
        <li ,(replace-target-place-command -self- (make-instance 'dummy-component-with-environment-restoring-error)
                                           "Display a component which errors while restoring its environment; in an AJAX request"
                                           :ajax #t)>
        <li <b "Synchronous/asynchronous behavior">>
        <li ,(command slow-js-logging-ajax-action
                      "JavaScript code returned in a slow AJAX request logs something to the JavaScript console. Synchronous."
                      :ajax #t :sync #t)>
        <li ,(command slow-js-logging-ajax-action
                      "JavaScript code returned in a slow AJAX request logs something to the JavaScript console. Asynchronous."
                      :ajax #t :sync #f)>>))))
