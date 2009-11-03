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

(def (function e) reset-root-component (&optional (frame *frame*))
  (setf (root-component-of frame) nil))

(def (function e) make-debug-menu ()
  (menu-item/widget ()
      "Debug"
    (menu-item/widget ()
        (command/widget (:send-client-state #f)
          "Start over (frame)"
          (make-action (reset-root-component))))
    (menu-item/widget ()
        "Toggle"
      (menu-item/widget ()
          (command/widget ()
            "Hierarchy (frame)"
            (make-action (toggle-debug-component-hierarchy))))
      (menu-item/widget ()
          (command/widget ()
            "Test mode (application)"
            (make-action (toggle-running-in-test-mode))))
      (menu-item/widget ()
          (command/widget ()
            "Ajax (application)"
            (make-action (toggle-ajax-enabled))))
      (menu-item/widget ()
          (command/widget ()
            "Profiling (server)"
            (make-action (toggle-profile-request-processing))))
      (menu-item/widget ()
          (command/widget ()
            "Debug server side (globally)"
            (make-action (notf *debug-on-error*))))
      (menu-item/widget ()
          (command/widget ()
            "Debug server side (application)"
            (make-action (toggle-debug-server-side))))
      (menu-item/widget ()
          (command/widget ()
            "Debug client side (frame)"
            (make-action (toggle-debug-client-side)))))
    (menu-item/widget ()
        "Inspect"
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Server"
            (make-value-inspector *server* :initial-alternative-type 't/detail/presentation)))
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Application"
            (make-value-inspector *application* :initial-alternative-type 't/detail/presentation)))
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Session"
            (make-value-inspector *session* :initial-alternative-type 't/detail/presentation)))
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Frame"
            (make-value-inspector *frame* :initial-alternative-type 't/detail/presentation)))
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Request"
            (make-value-inspector *request* :initial-alternative-type 't/detail/presentation)))
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Response"
            (make-value-inspector *response* :initial-alternative-type 't/detail/presentation))))
    (menu-item/widget ()
        "Miscellaneous"
      (menu-item/widget ()
          (command/widget ()
            "Action time error"
            (make-action
              (error "This is a demo error for testing purposes. It was signalled from the body of an action."))))
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Render time error"
            (inline-render-component/widget ()
              (error "This is a demo error for testing purposes. It was signalled from the render method of a component."))))
      (menu-item/widget ()
          ;; from http://turtle.dojotoolkit.org/~david/recss.html
          (inline-render-component/widget ()
            <a (:href "#"
                :class "command"
                :onClick `js-inline(wui.reload-css))
               "Reload CSS">))
      #+sbcl
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Frame size breakdown"
            (make-instance 'frame-size-breakdown))))))
