;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; command/widget

(def (component e) command/widget (widget/style content/mixin)
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
;; TODO: refactor this macro so that subclasses can reuse the code here
(def (macro e) command/widget ((&key (enabled #t) (visible #t) (default #f) (ajax #t ajax-provided?) js scheme path application-relative-path
                                     (delayed-content nil delayed-content-provided?)
                                     (send-client-state #t send-client-state-provided?))
                                &body content-and-action)
  ;; &body here is only for nicer indenting in emacs. content and action should be mandatory arguments, consider dropping &body...
  (assert (<= 1 (length content-and-action) 2))
  (bind ((content (first content-and-action))
         (action (second content-and-action)))
    (once-only (content action)
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
                            :ajax ,(if ajax-provided?
                                       ajax
                                       `(typep ,action 'action))
                            :js ,js
                            :action-arguments ,action-arguments)))))))

(def render-component :in passive-layer :around command/widget
  (values))

(def render-text command/widget
  (render-component (content-of -self-)))

(def render-xhtml command/widget
  (bind (((:read-only-slots content action enabled-component default ajax js action-arguments id) -self-)
         (send-client-state? (prog1
                                 (getf action-arguments :send-client-state #t)
                               (remove-from-plistf action-arguments :send-client-state))))
    (render-command/xhtml action content
                          :id id :style-class (component-style-class -self-) :action-arguments action-arguments
                          :js js :enabled enabled-component :default default :ajax ajax
                          :send-client-state send-client-state?)))

(def function render-command/xhtml (action content &key (id (generate-unique-component-id))
                                           style-class action-arguments js
                                           (enabled #t) default (ajax (typep action 'action))
                                           (send-client-state #t) (sync #t))
  (if (force enabled)
      (bind ((name (when (running-in-test-mode? *application*)
                     (if (typep content 'icon/widget)
                         (symbol-name (name-of content))
                         (princ-to-string content))))
             (submit-id nil))
        ;; NOTE name is not a valid attribute in xhtml, but in test mode it's rendered to help test code finding commands
        ;; TODO: if we render it as a span, then tab navigation skips the commands
        <span (:id ,id :class ,style-class ,(maybe-make-xml-attribute "name" name))
          #\Newline ;; NOTE: this is mandatory for chrome when the element does not have a content
          ,(render-component content)>
        (when default
          (setf submit-id (generate-unique-component-id))
          <input (:id ,submit-id :type "submit" :style "display: none;")>)
        (render-command-js-event-handler "onclick" (if submit-id (list id submit-id) id) action
                                         :js js :ajax (force ajax) :sync sync
                                         :action-arguments action-arguments
                                         :send-client-state send-client-state))
      <span (:id ,id :class "command widget disabled")
        #\Newline ;; NOTE: this is mandatory for chrome when the element does not have a content
        ,(render-component content)>))

(def function render-command-js-event-handler (event-name id action &key action-arguments js
                                                          (ajax (typep action 'action))
                                                          (send-client-state #t) (sync #t))
  ;; TODO the name 'ajax' doesn't really suggest that it may also be a dom id... add an explicit target-dom-node argument all the way up
  ;; TODO and then probably delete this function and call render-action-js-event-handler directly...
  (check-type ajax (or boolean string))
  (render-action-js-event-handler event-name id action :action-arguments action-arguments :js js
                                  :target-dom-node (when (stringp ajax) ajax) :ajax (to-boolean ajax)
                                  :send-client-state send-client-state :sync sync))

(def (function e) render-command-onclick-handler (command id)
  (bind ((action (action-of command))
         (action-arguments (action-arguments-of command))
         (send-client-state? (prog1
                                 (getf action-arguments :send-client-state #t)
                               (remove-from-plistf action-arguments :send-client-state))))
    (render-command-js-event-handler "onclick" id action
                                     :js (js-of command)
                                     :ajax (force (ajax-of command))
                                     :action-arguments action-arguments
                                     :send-client-state send-client-state?)))

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

(def method command-position ((self command/widget))
  (command-position (content-of self)))

(def method component-style-class ((self command/widget))
  (if (eq 'command/widget (class-name (class-of self)))
      (call-next-method)
      (string+ (call-next-method) " command")))

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
  "The BACK command puts REPLACEMENT-COMPONENT and ORIGINAL-COMPONENT back to their COMPONENT-PLACEs and removes itself from its COMMAND-BAR"
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
        (when replacement-place
          (setf (parent-component-of replacement-component) nil))
        (setf (component-at-place original-place) replacement-component)))))
