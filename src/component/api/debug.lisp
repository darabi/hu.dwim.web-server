;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Debug component hierarchy

(def method supports-debug-component-hierarchy? ((self component))
  #t)

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
       (with-output-to-sequence (buffer-stream :external-format +default-encoding+ :initial-buffer-size 256)
         (emit-into-xml-stream buffer-stream
           `xml,@(with-collapsed-js-scripts
                  (with-dojo-widget-collector
                    (-body-)))
           +void+))
       :encoding +default-encoding+))))

(def (function e) render-to-xhtml-string (component &key (ajax-aware #f))
  (bind ((*ajax-aware-request* ajax-aware))
    (with-render-to-xhtml-string-context
      (call-in-rendering-environment *application* *session*
                                     (lambda ()
                                       (ajax-aware-render component))))))