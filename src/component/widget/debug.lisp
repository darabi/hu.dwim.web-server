;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Debug menu

(def (function e) reset-frame-root-component ()
  (setf (root-component-of *frame*) nil))

(def (function e) toggle-running-in-test-mode (application)
  (notf (running-in-test-mode? application)))

(def (function e) toggle-ajax-enabled (application)
  (notf (ajax-enabled? application)))

(def (function e) toggle-profile-request-processing (&optional (server *server*))
  (notf (profile-request-processing? server)))

(def (function e) toggle-debug-component-hierarchy (&optional (frame *frame*))
  (notf (debug-component-hierarchy-p frame)))

(def (function e) toggle-debug-client-side (&optional (frame *frame*))
  (notf (debug-client-side? (root-component-of frame))))

(def (function e) make-debug-menu-item ()
  (menu-item/widget ()
      "Debug"
    (menu-item/widget ()
        (command/widget (:send-client-state #f)
          "Start over"
          (make-action (reset-frame-root-component))))
    (menu-item/widget ()
        "Toggle"
      (menu-item/widget ()
          (command/widget ()
            "Test mode"
            (make-action (toggle-running-in-test-mode))))
      (menu-item/widget ()
          (command/widget ()
            "Profiling"
            (make-action (toggle-profile-request-processing))))
      (menu-item/widget ()
          (command/widget ()
            "Hierarchy"
            (make-action (toggle-debug-component-hierarchy))))
      (menu-item/widget ()
          (command/widget ()
            "Debug client side"
            (make-action (toggle-debug-client-side))))
      (menu-item/widget ()
          (command/widget ()
            "Toggle AJAX"
            (make-action (toggle-ajax-enabled *application*)))))
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
            (make-value-inspector *frame*))))
    (menu-item/widget ()
        "Miscellaneous"
      (menu-item/widget ()
          (command/widget ()
            "Action time error"
            (make-action
              (error "Testing error handling, error was produced during handling an action"))))
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Render time error"
            (inline/widget
              (error "Testing error handling, error was produced during rendering a component"))))
      (menu-item/widget ()
          ;; from http://turtle.dojotoolkit.org/~david/recss.html
          (inline/widget
            <a (:href "#"
                :class "command"
                :onClick `js-inline(wui.reload-css))
               "Reload CSS">))
      #+sbcl
      (menu-item/widget ()
          (replace-target-place/widget ()
              "Frame size breakdown"
            (make-instance 'frame-size-breakdown))))))
