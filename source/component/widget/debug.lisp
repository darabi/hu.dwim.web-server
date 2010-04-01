;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Debug menu

(def (function e) toggle-profile-request-processing (&optional (server *server*))
  (notf (profile-request-processing? server)))

(def (function e) toggle-running-in-test-mode (&optional (application *application*))
  (notf (running-in-test-mode? application)))

(def (function e) toggle-ajax-enabled (&optional (application *application*))
  (notf (ajax-enabled? application)))

(def (function e) toggle-debug-component-hierarchy (&optional (frame *frame*))
  (notf (debug-component-hierarchy? frame)))

(def (function e) toggle-debug-server-side (&optional (application *application*))
  (if (slot-boundp application 'debug-on-error)
      (notf (slot-value application 'debug-on-error))
      (setf (slot-value application 'debug-on-error) (not *debug-on-error*))))

(def (function e) toggle-debug-client-side (&optional (frame *frame*))
  (notf (debug-client-side? (root-component-of frame))))

(def (function e) clear-root-component (&optional (frame *frame*))
  (setf (root-component-of frame) nil))

(def (function e) make-debug-menu ()
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
        "Toggle"
      (menu-item/widget ()
          (command/widget (:ajax #f)
            "Hierarchy (frame)"
            (make-action (toggle-debug-component-hierarchy))))
      (menu-item/widget ()
          (command/widget (:ajax #f)
            "Test mode (application)"
            (make-action (toggle-running-in-test-mode))))
      (menu-item/widget ()
          (command/widget (:ajax #f)
            "Ajax (application)"
            (make-action (toggle-ajax-enabled))))
      (menu-item/widget ()
          (command/widget (:ajax #f)
            "Profiling (server)"
            (make-action (toggle-profile-request-processing))))
      (menu-item/widget ()
          (command/widget (:ajax #f)
            "Debug server side (globally)"
            (make-action (notf *debug-on-error*))))
      (menu-item/widget ()
          (command/widget (:ajax #f)
            "Debug server side (application)"
            (make-action (toggle-debug-server-side))))
      (menu-item/widget ()
          (command/widget (:ajax #f)
            "Debug client side (frame)"
            (make-action (toggle-debug-client-side)))))
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
            "Error handling tests..."
          (make-instance 'client-side-error-handling-test)))))

;;;;;;
;;; Debug menu

(def (component e) client-side-error-handling-test (widget/style)
  ())

(def render-xhtml client-side-error-handling-test
  (with-render-style/abstract (-self-)
    (bind ((error-action (make-action
                           (error "This is a demo error for testing purposes. It was signalled from the body of an action.")))
           (slow-js-logging-ajax-action (make-action
                                          (sleep 2)
                                          (make-functional-response/ajax-aware-client ()
                                            <script `js-inline(log.info "After some delay on the server, we were rendered back...")>))))
     (flet ((command (action content &rest args)
              (apply 'render-command/xhtml action content :style-class "command" args))
            (replace-target-place-command (component replacement-component content &rest args)
              (apply 'render-replace-target-place-command/xhtml component replacement-component content :style-class "command" args))
            (make-inline-component-with-rendering-error ()
              (bind ((us nil))
                (setf us (inline-render-component/widget ()
                           (error "This is a demo error signaled from the render method of the inline component ~A" us))))))
       <p "These actions help testing how errors in different situations are dealt with.">
       <ul
        <li <b "JavaScript errors">>
        <li ,(command nil "OnClick calls undefined function" :js (lambda () `js(this-function-is-undefined)))>
        <li ,(command nil "OnClick throws 'foo'" :js (lambda () `js(throw "foo")))>
        <li ,(command (make-action
                        (make-functional-response/ajax-aware-client ()
                          <script `js-inline(this-function-is-undefined)>))
                      "JavaScript code returned in an AJAX answer calls an undefined function"
                      :ajax #t)>
        <li <b "Errors in actions (check your server side debugger when debugging is turned on)">>
        <li ,(command error-action "Action time error in a full page reload" :ajax #f)>
        <li ,(command error-action "Action time error in an AJAX request" :ajax #t)>
        <li <b "Errors in render methods">>
        <li ,(replace-target-place-command -self- (make-inline-component-with-rendering-error)
                                           "Render time error in a full page reload"
                                           :ajax #f)>
        <li ,(replace-target-place-command -self- (make-inline-component-with-rendering-error)
                                           "Render time error in an AJAX request"
                                           :ajax #T)>
        <li <b "Synchronous/asynchronous behavior">>
        <li ,(command slow-js-logging-ajax-action
                      "JavaScript code returned in a slow AJAX request logs something to the JavaScript console. Synchronous."
                      :ajax #t :sync #t)>
        <li ,(command slow-js-logging-ajax-action
                      "JavaScript code returned in a slow AJAX request logs something to the JavaScript console. Asynchronous."
                      :ajax #t :sync #f)>>))))
