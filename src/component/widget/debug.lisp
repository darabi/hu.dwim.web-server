;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Debug menu

(def (function e) reset-frame-root-component ()
  (setf (root-component-of *frame*) nil))

(def (function e) toggle-running-in-test-mode (&optional (application *application*))
  (notf (running-in-test-mode? application)))

(def (function e) toggle-profile-request-processing (&optional (server *server*))
  (notf (profile-request-processing? server)))

(def (function e) toggle-debug-component-hierarchy (&optional (frame *frame*))
  (notf (debug-component-hierarchy-p frame)))

(def (function e) toggle-debug-client-side (&optional (frame *frame*))
  (notf (debug-client-side? (root-component-of frame))))

(def (function e) make-debug-menu ()
  (menu-item ()
      "Debug"
    (menu-item ()
        (command (:send-client-state #f)
          "Start over"
          (make-action (reset-frame-root-component))))
    (menu-item ()
        (command ()
          "Toggle test mode"
          (make-action (toggle-running-in-test-mode))))
    (menu-item ()
        (command ()
          "Toggle profiling"
          (make-action (toggle-profile-request-processing))))
    (menu-item ()
        (command ()
          "Toggle hierarchy"
          (make-action (toggle-debug-component-hierarchy))))
    (menu-item ()
        (command ()
          "Toggle debug client side"
          (make-action (toggle-debug-client-side))))
    (menu-item ()
        ;; from http://turtle.dojotoolkit.org/~david/recss.html
        (inline-render
          <a (:href "#"
                    :class "command"
                    :onClick `js-inline(wui.reload-css))
             "Reload CSS">))
    #+sbcl
    (menu-item ()
        (replace-menu-target-command ()
          "Frame size breakdown"
          (make-instance 'frame-size-breakdown)))
    (menu-item ()
        (replace-menu-target-command ()
          "Server"
          (make-inspector *server*)))
    (menu-item ()
        (replace-menu-target-command ()
          "Application"
          (make-inspector *application*)))
    (menu-item ()
        (replace-menu-target-command ()
          "Session"
          (make-inspector *session*)))
    (menu-item ()
        (replace-menu-target-command ()
          "Frame"
          (make-inspector *frame*)))))
