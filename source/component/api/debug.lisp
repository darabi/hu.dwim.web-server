;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

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
            ,(bind ((*debug-component-hierarchy* #f))
               (render-component (make-copy-to-repl-command -self-))
               (render-component (make-inspect-in-repl-command -self-)))
            ,(call-next-method)>
          (call-next-method))
    (skip-rendering-component ()
      :report (lambda (stream)
                (format stream "Skip rendering ~A and put an error marker in place" -self-))
      <div (:class "rendering-error")
        "Error during rendering "
        ,(bind ((*print-level* 1))
           (princ-to-string -self-))>)))

(def render-xhtml null
  (restart-case
      (error "For debugging purposes NIL is not a valid COMPONENT, you may want to use EMPTY/LAYOUT or something similar.")
    (skip-rendering-component ()
      :report (lambda (stream)
                (format stream "Skip rendering NIL and put an error marker in place"))
      <div (:class "rendering-error")
        "Cannot render NIL, it is not a valid component">)))

(def function inspect-in-repl (component)
  (awhen (or swank::*emacs-connection*
             (swank::default-connection))
    (swank::with-connection (it)
      (bind ((swank::*buffer-package* *package*)
             (swank::*buffer-readtable* *readtable*))
        (swank::inspect-in-emacs component)))))

(def function copy-to-repl (component)
  (awhen (or swank::*emacs-connection*
             (swank::default-connection))
    (swank::with-connection (it)
      (swank::present-repl-results (list component)))))

;;;;;;
;;; Render to string

;; TODO rename to with-fake-render-context
(def (with-macro e) with-render-context ()
  (bind ((*request* (if (boundp '*request*)
                        *request*
                        (make-instance 'request :uri (parse-uri ""))))
         (*response* (if (boundp '*response*)
                         *response*
                         (make-instance 'response)))
         (*application* (if (boundp '*application*)
                            *application*
                            (make-instance 'application :path-prefix "")))
         (*session* (if (boundp '*session*)
                        *session*
                        (aprog1 (make-instance 'session)
                          (setf (id-of it) "1234567890"))))
         (*frame* (if (boundp '*frame*)
                      *frame*
                      (make-instance 'frame :session *session*)))
         (*rendering-phase-reached* #f))
    (with-lock-held-on-session (*session*)
      (-body-))))

;; TODO rename to with-fake-render-context/emit-xml-into-string, or something like that
(def (with-macro e) with-render-to-string-context ()
  (with-render-context
    (octets-to-string
     (with-output-to-sequence (buffer-stream :external-format +default-encoding+ :initial-buffer-size 256)
       (emit-into-xml-stream buffer-stream
         `xml,@(with-xhtml-body-environment ()
                 (-with-macro/body-))
         +void+))
     :encoding +default-encoding+)))

(def (function e) render-to-xhtml-string (component &key (ajax-aware #f))
  (bind ((*ajax-aware-request* ajax-aware))
    (with-render-to-string-context
      (call-in-rendering-environment *application* *session*
                                     (lambda ()
                                       (ajax-aware-render (force component)))))))
