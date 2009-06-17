;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Debug component hierarchy

(def render-xhtml :around component
  (restart-case
      (if (and *debug-component-hierarchy*
               (supports-debug-component-hierarchy? -self-))
          <div (:class "debug-component-hierarchy")
            <span (:class "class-name") ,(instance-class-name-as-string -self-)>
            <span <a (:href ,(register-action/href (make-copy-to-repl-action -self-))) "COPY">
                  " "
                  <a (:href ,(register-action/href (make-inspect-in-repl-action -self-))) "INSPECT">>
            ,(call-next-method)>
          (call-next-method))
    (skip-rendering-component ()
      :report (lambda (stream)
                (format stream "Skip rendering ~A and put an error marker in place" -self-))
      <div (:class "rendering-error") "Error during rendering "
        ,(bind ((*print-level* 1))
           (princ-to-string -self-))>)))

(def function make-inspect-in-repl-action (component)
  (make-action
    (awhen (or swank::*emacs-connection*
               (swank::default-connection))
      (swank::with-connection (it)
        (bind ((swank::*buffer-package* *package*)
               (swank::*buffer-readtable* *readtable*))
          (swank::inspect-in-emacs component))))))

(def function make-copy-to-repl-action (component)
  (make-action
    (awhen (or swank::*emacs-connection*
               (swank::default-connection))
      (swank::with-connection (it)
        (swank::present-in-emacs component)
        (swank::present-in-emacs #.(string #\Newline))))))

;;;;;;
;;; Render to string

(def (with-macro e) with-render-to-xhtml-string-context ()
  (bind ((*request* (make-instance 'request :uri (parse-uri "")))
         (*response* (make-instance 'response))
         (*application* (make-instance 'application :path-prefix ""))
         (*session* (make-instance 'session))
         (*frame* (make-instance 'frame :session *session*))
         (*rendering-phase-reached* #f))
    (setf (id-of *session*) "1234567890")
    (with-lock-held-on-session (*session*)
      (octets-to-string
       (with-output-to-sequence (buffer-stream :external-format +encoding+ :initial-buffer-size 256)
         (emit-into-xml-stream buffer-stream
           `xml,@(with-collapsed-js-scripts
                  (with-dojo-widget-collector
                    (-body-)))
           +void+))
       :encoding +encoding+))))

(def (function e) render-to-xhtml-string (component &key (ajax-aware #f))
  (bind ((*ajax-aware-request* ajax-aware))
    (with-render-to-xhtml-string-context
      (call-in-rendering-environment *application* *session*
                                     (lambda ()
                                       (ajax-aware-render component))))))

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
          (make-instance 'frame-size-breakdown-component)))
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
