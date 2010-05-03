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
    (ajax-enabled? *application*)
    :type boolean
    :documentation "TRUE means the action supports ajax, but there will be no precise indication at the remote side.")
   (subject-component
    nil
    :type (or null t) ; FIXME :type (or null component) would try to set the parent-component slot of components that are put in this slot. we obviously don't want that...
    :documentation "Specifies the component which will be used to indicate the progress when the command is issued.")
   (action
    :type (or uri action)
    :documentation "The action (function) that will be called when this command is activated.")
   (action-arguments
    nil
    :type list)
   (js
    nil
    :type t)))

;; TODO: refactor this macro so that subclasses can reuse the code here
(def (macro e) command/widget ((&key (enabled #t) (visible #t) (default #f) (ajax #t ajax-provided?) subject-component
                                     js scheme path application-relative-path
                                     (delayed-content nil delayed-content-provided?)
                                     (send-client-state #t send-client-state-provided?))
                                &body content-and-action)
  ;; KLUDGE &body here is only for nicer indenting in emacs. content and action should be mandatory arguments, consider dropping &body...
  (assert (<= 1 (length content-and-action) 2))
  (bind ((content (first content-and-action))
         (action (second content-and-action)))
    (once-only (content action)
      (unless ajax-provided?
        (setf ajax `(typep ,action 'action)))
      (with-unique-names (action-arguments)
        (flet ((maybe-push (key expression)
                 (when expression
                   `((awhen ,expression
                       (push it ,action-arguments)
                       (push ,key ,action-arguments))))))
          `(bind ((,action-arguments (list ,@(when delayed-content-provided?
                                                   `(:delayed-content ,delayed-content))
                                           ,@(when send-client-state-provided?
                                                   `(:send-client-state ,send-client-state)))))
             (debug-only (assert ,content () "Command factory called without a valid CONTENT"))
             ,@(maybe-push :scheme scheme)
             ,@(maybe-push :path path)
             ,@(maybe-push :application-relative-path application-relative-path)
             (make-instance 'command/widget
                            :content ,content
                            :action ,action
                            :enabled ,enabled
                            :visible ,visible
                            :default ,default
                            :ajax ,ajax
                            :subject-component ,subject-component
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
                          :id id
                          :style-class (component-style-class -self-)
                          :action-arguments action-arguments
                          :js js
                          :enabled enabled-component
                          :default default
                          :subject-dom-node (subject-dom-node-for-command -self-)
                          :ajax (to-boolean (force ajax))
                          :send-client-state send-client-state?)))

(def function render-command/xhtml (action content &key (id (generate-unique-component-id))
                                           style-class action-arguments js (enabled #t) default
                                           subject-dom-node (ajax (typep action 'action))
                                           (send-client-state #t) (sync #t))
  (check-type ajax boolean)
  (check-type subject-dom-node (or null string))
  (check-type id string)
  (if (force enabled)
      (bind ((name (when (running-in-test-mode? *application*)
                     (if (typep content 'icon/widget)
                         (symbol-name (name-of content))
                         (princ-to-string content))))
             (submit-id (when default
                          (generate-unique-component-id))))
        ;; NOTE name is not a valid attribute in xhtml, but in test mode it's rendered to help test code finding commands
        ;; TODO: if we render it as a span, then tab navigation skips the commands
        <span (:id ,id :class ,style-class ,(maybe-make-xml-attribute "name" name))
          #\Newline ;; NOTE: this is mandatory for chrome when the element does not have a content
          ,(render-component content)>
        (render-action-js-event-handler "onclick" (if submit-id (list id submit-id) id) action
                                        :action-arguments action-arguments
                                        :js js
                                        :subject-dom-node subject-dom-node
                                        :ajax ajax
                                        :send-client-state send-client-state
                                        :sync sync)
        (when default
          ;; NOTE: we must do this after render-action-js-event-handler, because the action gets registered in it and we use its id here
          ;; TODO add client side warning for multiple default actions?
          <input (:id ,submit-id :type "submit" :style "display: none;"
                  :name #.+action-id-parameter-name+ :value ,(when (typep action 'action)
                                                               (id-of action)))>
          (when (and (not *frame*)
                     (typep action 'uri))
            ;; this is needed on Chrome, which doesn't call onclick on the submit input dom node
            `js(setf (slot-value (aref document.forms 0) 'action) ,(print-uri-to-string action)))))
      <span (:id ,id :class `str("disabled " ,style-class))
        #\Newline ;; NOTE: this is mandatory for chrome when the element does not have a content
        ,(render-component content)>))

(def function subject-dom-node-for-command (command)
  (bind ((subject-component (force (subject-component-of command))))
    (etypecase subject-component
      (id/mixin (id-of subject-component))
      ;; TODO if there's parent/mixin we could find an id/mixin on the parent chain... but should we?
      ((or null component) nil))))

(def (function e) render-command-onclick-handler (command target-id)
  (bind (((:read-only-slots action ajax js action-arguments) command)
         (send-client-state? (prog1
                                 (getf action-arguments :send-client-state #t)
                               (remove-from-plistf action-arguments :send-client-state))))
    (render-action-js-event-handler "onclick" target-id action
                                    :action-arguments action-arguments
                                    :js js
                                    :subject-dom-node (subject-dom-node-for-command command)
                                    :ajax (to-boolean (force ajax))
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
      (string+ "button-border " (call-next-method))
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
                                          `(:visible ,(delay (and (not (has-edited-descendant-component? replacement-component))
                                                                  (eq (force replacement-component) (component-at-place original-place)))))))))
        (push-command back-command replacement-component)
        (when replacement-place
          (setf (parent-component-of replacement-component) nil))
        (setf (component-at-place original-place) replacement-component)))))
