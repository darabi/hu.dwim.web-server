;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; command/widget

(def (component e) command/widget (widget/basic content/mixin disableable/mixin)
  (;; TODO: put a lambda with the authorization rule captured here in hu.dwim.perec integration
   ;; TODO: always wrap the action lambda with a call to execute-command
   (available
    #t
    :type boolean)
   (default
    #f
    :type boolean
    :documentation "TRUE means the action will execute on pressing enter.")
   (ajax
    #f
    :type (or boolean string)
    :documentation "TRUE means the action supports ajax, but there will be no precise indication at the remote side. A string specifies the id of the component which will be used to indicate the processing of the action.")
   (action
    :type (or uri action)
    :documentation "The action (function) that will be called when this command is activated.")
   (action-arguments
    nil
    :type list)
   (js
    nil
    :type t)))

;; TODO: don't generate such a big code if possible
(def (macro e) command/widget ((&key (enabled #t) (visible #t) (default #f) (ajax #f) js scheme path application-relative-path
                                     (delayed-content nil delayed-content-provided?)
                                     (send-client-state #t send-client-state-provided?))
                                &body content-and-action)
  (assert (length= 2 content-and-action))
  (bind ((content (first content-and-action))
         (action (second content-and-action)))
    (once-only (content)
      (with-unique-names (action-arguments)
        (flet ((maybe-push (key expression)
                 (when expression
                   `(awhen ,expression
                      (push it ,action-arguments)
                      (push ,key ,action-arguments)))))
          `(bind ((,action-arguments ()))
             (debug-only (assert ,content () "Command factory called without a valid CONTENT"))
             ,(when delayed-content-provided?
                    `(progn
                       (push ,delayed-content ,action-arguments)
                       (push :delayed-content ,action-arguments)))
             ,(when send-client-state-provided?
                    `(progn
                       (push ,send-client-state ,action-arguments)
                       (push :send-client-state ,action-arguments)))
             ,(maybe-push :scheme scheme)
             ,(maybe-push :path path)
             ,(maybe-push :application-relative-path application-relative-path)
             (make-instance 'command/widget
                            :content ,content
                            :action ,action
                            :enabled ,enabled
                            :visible ,visible
                            :default ,default
                            :ajax ,ajax
                            :js ,js
                            :action-arguments ,action-arguments)))))))

(def render-xhtml command/widget
  (bind (((:read-only-slots content action enabled-component default ajax js action-arguments) -self-))
    (render-command content action :enabled enabled-component :default default :ajax ajax :js js :action-arguments action-arguments)))

(def render-text command/widget
  (render-component (content-of -self-)))

(def render-component :in passive-layer command/widget
  (render-content-for -self-))

(def function href-for-command (action action-arguments)
  (bind ((send-client-state? (prog1
                                 (getf action-arguments :send-client-state #t)
                               (remove-from-plistf action-arguments :send-client-state)))
         (href (etypecase action
                 (action (apply 'register-action/href action action-arguments))
                 ;; TODO: wastes resources. store back the printed uri? see below also...
                 (uri (print-uri-to-string action)))))
    (values href send-client-state?)))

(def (function e) render-command (content action &key (enabled #t) (default #f) (ajax (not (null *frame*))) js action-arguments)
  (if (force enabled)
      (bind ((id (generate-response-unique-string))
             ((:values href send-client-state?) (href-for-command action action-arguments))
             (onclick-js (or js
                             (lambda (href)
                               `js(wui.io.action ,href
                                                 :event event
                                                 :ajax ,(when (ajax-enabled? *application*)
                                                          (force ajax))
                                                 :send-client-state ,send-client-state?))))
             (name (when (running-in-test-mode? *application*)
                     (if (typep content 'icon/widget)
                         (symbol-name (name-of content))
                         (princ-to-string content)))))
        ;; TODO: name is not a valid attribute but needed for test code to be able to find commands
        ;; TODO: when rendering a span, tab navigation skips the commands
        <span (:id ,id :class "command widget" :name ,name) ,(render-component content)>
        `js(on-load
            (dojo.connect (dojo.by-id ,id) "onclick" (lambda (event) ,(funcall onclick-js href)))
            (wui.setup-component ,id "command/widget"))
        ;; TODO: use dojo.connect for keyboard events
        (when default
          (bind ((submit-id (generate-response-unique-string)))
            <input (:id ,submit-id :type "submit" :style "display: none;")>
            `js(on-load (dojo.connect (dojo.by-id ,submit-id) "onclick" (lambda (event) ,(funcall onclick-js href)))))))
      <span (:class "command disabled") ,(render-component content)>))

(def (function e) render-command-onclick-handler (command id)
  (bind ((action (action-of command))
         (action-arguments (action-arguments-of command))
         ((:values href send-client-state?) (href-for-command action action-arguments)))
    `js(on-load (dojo.connect (dojo.by-id ,id) "onclick" nil
                              (lambda (event)
                                (wui.io.action ,href
                                               :event event
                                               :ajax,(when (ajax-enabled? *application*) (ajax-of command))
                                               :send-client-state ,send-client-state?))))))

(def (function e) execute-command (command)
  (bind ((executable? #t))
    (flet ((report-error (string)
             (add-component-error-message command string)
             (setf executable? #f)))
      (unless (available? command)
        (report-error #"execute-command.command-unavailable"))
      (unless (enabled-component? command)
        (report-error #"execute-command.command-disabled"))
      (unless (visible-component? command)
        (report-error #"execute-command.command-invisible"))
      (when executable?
        (funcall (action-of command))))))

;;;;;;
;;; Generic commands

(def (function e) make-replace-command (original-component replacement-component &rest replace-command-args)
  "The REPLACE command replaces ORIGINAL-COMPONENT with REPLACEMENT-COMPONENT"
  (assert (and original-component
               replacement-component))
  (apply #'make-instance 'command/widget
         :action (make-action
                   (execute-replace original-component replacement-component))
         replace-command-args))

(def function execute-replace (original-component replacement-component)
  (bind ((original-component (force original-component)))
    (with-restored-component-environment original-component
      (bind ((original-place (make-component-place original-component))
             (replacement-component (force replacement-component)))
        (setf (component-at-place original-place) replacement-component)))))

(def (function e) make-back-command (original-component original-place replacement-component replacement-place &rest args)
  "The BACK command puts REPLACEMENT-COMPONENT and ORIGINAL-COMPONENT back to their COMPONENT-PLACE and removes itself from its COMMAND-BAR"
  (prog1-bind command (apply #'make-instance 'command/widget args)
    (setf (action-of command) (make-action
                                (pop-command command)
                                (when original-place
                                  (setf (component-at-place original-place) (force original-component)))
                                (when replacement-place
                                  (setf (component-at-place replacement-place) (force replacement-component)))))))

(def (function e) make-replace-and-push-back-command (original-component replacement-component replace-command-args back-command-args)
  "The REPLACE command replaces ORIGINAL-COMPONENT with REPLACEMENT-COMPONENT and pushes a BACK command into the COMMAND-BAR of REPLACEMENT-COMPONENT to revert the changes"
  (assert (and original-component
               replacement-component))
  (apply #'make-instance 'command/widget
         :action (make-action
                   (execute-replace-and-push-back original-component replacement-component back-command-args))
         replace-command-args))

(def function execute-replace-and-push-back (original-component replacement-component back-command-args)
  (bind ((original-component (force original-component)))
    (with-restored-component-environment (parent-component-of original-component)
      (bind ((replacement-component (force replacement-component))
             (replacement-place (make-component-place replacement-component))
             (original-place (make-component-place original-component))
             (back-command (apply #'make-back-command original-component original-place replacement-component replacement-place
                                  (append back-command-args
                                          `(:visible ,(delay (and (not (has-edited-descendant-component-p replacement-component))
                                                                  (eq (force replacement-component) (component-at-place original-place)))))))))
        (push-command back-command replacement-component)
        (setf (component-at-place original-place) replacement-component)))))
