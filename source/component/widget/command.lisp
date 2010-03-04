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
  (assert (length= 2 content-and-action))
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

(def function %default-onclick-js (id ajax send-client-state?)
  (lambda (href)
    (bind ((ajax (and (ajax-enabled? *application*)
                      (force ajax))))
      ;; KLUDGE: this condition prevents firing obsolete actions, they are not necessarily
      ;;         removed by destroy when simply replaced by some other content, this may leak memory on the cleint side
      `js(when (dojo.by-id ,id)
           (wui.io.action ,href
                          :event event
                          :ajax ,(when (ajax-enabled? *application*)
                                       (force ajax))
                          :send-client-state ,send-client-state?))
      ;; TODO add a *special* that collects the args of all action's and runs a js side loop to process the literal arrays
      ;; TODO add special handling of apply to qq so that the 'this' arg of .apply is not needed below (wui.io.action twice)
      ;; TODO do something like this below instead of the above, once qq properly emits commas in the output of (create ,@emtpy-list)
      #+nil
      (if (and (eq ajax #t)
               send-client-state?)
          `js(wui.io.action ,href :event event)
          `js(.apply wui.io.action wui.io.action
                     (array ,href
                            (create
                             :event event
                             ,@(append (unless (eq ajax #t)
                                         (list (make-instance 'hu.dwim.walker:free-variable-reference-form :name :ajax)
                                               (make-instance 'hu.dwim.quasi-quote.js:js-unquote :form 'ajax)))
                                       (unless send-client-state?
                                         (list (make-instance 'hu.dwim.walker:free-variable-reference-form :name :send-client-state)
                                               (make-instance 'hu.dwim.walker:constant-form :value '|false|)))))))))))

(def render-xhtml command/widget
  ;; TODO the 'ajax' doesn't really suggest that it may also be a dom id...
  ;; FIXME theres quite some duplication with RENDER-COMMAND-ONCLICK-HANDLER
  (bind (((:read-only-slots content action enabled-component default ajax js action-arguments id) -self-)
         (style-class (component-style-class -self-)))
    (if (force enabled-component)
        (bind (((:values href send-client-state?) (href-for-command action action-arguments))
               (onclick-js (or js
                               (%default-onclick-js id ajax send-client-state?)))
               (name (when (running-in-test-mode? *application*)
                       (if (typep content 'icon/widget)
                           (symbol-name (name-of content))
                           (princ-to-string content)))))
          ;; TODO: name is not a valid attribute but needed for test code to be able to find commands
          ;; TODO: when rendering a span, tab navigation skips the commands
          <span (:id ,id :class ,style-class :name ,name)
                #\Newline ;; NOTE: this is mandatory for chrome when the element does not have a content
                ,(render-component content)>
          `js(on-load
              (dojo.connect (dojo.by-id ,id) "onclick" (lambda (event) ,(funcall onclick-js href))))
          ;; TODO: use dojo.connect for keyboard events
          (when default
            (bind ((submit-id (generate-unique-component-id)))
              <input (:id ,submit-id :type "submit" :style "display: none;")>
              `js(on-load (dojo.connect (dojo.by-id ,submit-id) "onclick" (lambda (event) ,(funcall onclick-js href)))))))
        <span (:id ,id :class "command widget disabled")
              #\Newline ;; NOTE: this is mandatory for chrome when the element does not have a content
              ,(render-component content)>)))

(def render-text command/widget
  (render-component (content-of -self-)))

(def render-passive command/widget
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

(def (function e) render-command-onclick-handler (command id)
  ;; FIXME share code with (def render-xhtml command/widget ...)
  (bind ((action (action-of command))
         (action-arguments (action-arguments-of command))
         ((:values href send-client-state?) (href-for-command action action-arguments))
         (onclick-js (or (js-of command)
                         (%default-onclick-js id (ajax-of command) send-client-state?))))
    `js(on-load (dojo.connect (dojo.by-id ,id) "onclick"
                              (lambda (event)
                                ,(funcall onclick-js href))))))

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
