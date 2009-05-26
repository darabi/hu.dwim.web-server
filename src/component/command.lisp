;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Passive components layer

(def (layer e) passive-components-layer ()
  ())

;;;;;;
;;; Command component

(def component command-component (content-mixin)
  ((enabled #t :type boolean)
   ;; TODO: put a lambda with the authorization rule captured here in cl-perec integration
   ;; TODO: always wrap the action lambda with a call to execute-command
   (available #t :type boolean)
   (default #f :type boolean)
   (ajax #f :type boolean)
   (action :type (or uri action))
   (action-arguments nil)
   (js nil)))

(def (macro e) command (content action &key (enabled #t) (visible #t) (default #f) (ajax #f) js scheme path application-relative-path
                                (delayed-content nil delayed-content-provided?)
                                (send-client-state #t send-client-state-provided?))
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
           (make-instance 'command-component
                          :content ,content
                          :action ,action
                          :enabled ,enabled
                          :visible ,visible
                          :default ,default
                          :ajax ,ajax
                          :js ,js
                          :action-arguments ,action-arguments))))))

(def render-xhtml command-component
  (bind (((:read-only-slots content action enabled default ajax js action-arguments) -self-))
    (render-command content action :enabled enabled :default default :ajax ajax :js js :action-arguments action-arguments)))

(def render :in passive-components-layer command-component
  (render (content-of -self-)))

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
                     (if (typep content 'icon-component)
                         (symbol-name (name-of content))
                         (princ-to-string content)))))
        ;; TODO: name is not a valid attribute but needed for test code to be able to find commands
        ;; TODO: when rendering a span, tab navigation skips the commands
        <span (:id ,id :class "command" :name ,name) ,(render content)>
        `js(on-load
            (dojo.connect (dojo.by-id ,id) "onclick" (lambda (event) ,(funcall onclick-js href)))
            (wui.setup-component ,id "command-component"))
        ;; TODO: use dojo.connect for keyboard events
        (when default
          (bind ((submit-id (generate-response-unique-string)))
            <input (:id ,submit-id :type "submit" :style "display: none;")>
            `js(on-load (dojo.connect (dojo.by-id ,submit-id) "onclick" (lambda (event) ,(funcall onclick-js href)))))))
      <span (:class "command disabled") ,(render content)>))

(def (function e) render-command-onclick-handler (command id)
  (bind ((action (action-of command))
         (action-arguments (action-arguments-of command))
         ((:values href send-client-state?) (href-for-command action action-arguments)))
    `js(on-load (dojo.connect (dojo.by-id ,id) "onclick" nil
                              (lambda (event)
                                (wui.io.action ,href
                                               :event event
                                               :ajax,(when (ajax-enabled? *application*)
                                                      (force (ajax-p command)))
                                               :send-client-state ,send-client-state?))))))

(def (function e) execute-command (command)
  (bind ((executable? #t))
    (flet ((report-error (string)
             (add-user-error command string)
             (setf executable? #f)))
      (unless (force (available-p command))
        (report-error #"execute-command.command-unavailable"))
      (unless (force (enabled-p command))
        (report-error #"execute-command.command-disabled"))
      (unless (force (visible-p command))
        (report-error #"execute-command.command-invisible"))
      (when executable?
        (funcall (action-of command))))))

;;;;;;
;;; Command bar component

(def component command-bar-component ()
  ((commands nil :type components)))

(def (macro e) command-bar (&body commands)
  `(make-instance 'command-bar-component :commands (optional-list ,@commands)))

(def render-xhtml command-bar-component
  (bind (((:read-only-slots parent-component commands) -self-)
         (sorted-commands (sort-commands parent-component commands)))
    (setf (commands-of -self-) sorted-commands)
    (render-horizontal-list sorted-commands :css-class "command-bar")))

(def render-csv command-bar-component
  (render-csv-separated-elements #\Space (commands-of -self-)))

(def render :in passive-components-layer command-bar-component
  (values))

(def generic find-command-bar (component)
  (:method ((component component))
    (map-child-components component
                          (lambda (child)
                            (when (typep child 'command-bar-component)
                              (return-from find-command-bar child))))))

(def generic sort-commands (component commands)
  (:method ((component component) commands)
    (sort commands #'<
          :key (lambda (command)
                 ;; TODO: can't we make it faster/better (or a generic methodor something?)
                 (bind ((content (content-of command)))
                   (or (when (typep content 'icon-component)
                         (position (name-of (content-of command))
                                   '(answer back focus-out open-in-new-frame focus-in collapse collapse-all expand-all refresh edit save cancel store revert new delete)
                                   :test #'eq))
                       most-positive-fixnum))))))

(def (function e) push-command (command component)
  "Push a new COMMAND into the COMMAND-BAR of COMPONENT"
  ;; FIXME: TODO: KLUDGE: command-bar is usually created from refresh and is not available after make-instance
  (ensure-uptodate component)
  (bind ((command-bar (find-command-bar component)))
    (assert command-bar nil "No command bar found, no place to push ~A in ~A" command component)
    (setf (commands-of command-bar)
          (sort-commands component (cons command (commands-of command-bar))))))

(def function pop-command (command)
  "Pop the COMMAND from the container COMMAND-BAR"
  (removef (commands-of (parent-component-of command)) command))

;;;;;
;;; Popup command menu

(def component popup-menu-component (content-mixin style-mixin remote-identity-mixin)
  ((target nil)
   (commands nil :type components)))

(def icon show-context-menu :label nil)
(def resources hu
  (icon-tooltip.show-context-menu "Környezetfüggő menü megjelenítése"))
(def resources en
  (icon-tooltip.show-context-menu "Show context menu"))

(def render-xhtml popup-menu-component
  (bind (((:read-only-slots target commands id css-class style) -self-)
         (menu-id (generate-response-unique-string)))
    (when commands
      <span (:id ,id :class ,css-class :style ,style)
          ,(call-next-method)
          ,(render-dojo-widget (menu-id)
             <div (:id ,menu-id
                   :dojoType #.+dijit/menu+
                   :targetNodeIds ,(if target
                                       (id-of target)
                                       id)
                   :style "display: none;")
               ,(iter (for command :in commands)
                      (for command-id = (generate-frame-unique-string))
                      (when (force (visible-p command))
                        (render-dojo-widget (command-id)
                          <div (:id ,command-id
                                :dojoType #.+dijit/menu-item+
                                :iconClass ,(icon-class (name-of (content-of command))))
                            ,(render-icon :icon (content-of command) :class nil)>
                          (render-command-onclick-handler command command-id))))>)>
      `js(on-load
          (wui.setup-component ,menu-id "popup-menu-component")))))

(def render-csv popup-menu-component
  (render-csv-separated-elements #\Space (commands-of -self-)))

;;;;;;
;;; Context menu mixin

(def component context-menu-mixin ()
  ((context-menu :type component))
  (:documentation "A component with a context menu"))

(def refresh context-menu-mixin
  (bind ((class (component-dispatch-class -self-))
         (prototype (when class (class-prototype class))))
    (setf (context-menu-of -self-) (make-context-menu -self- class prototype (component-value-of -self-)))))

(def (layered-function e) make-context-menu (component class prototype value)
  (:method ((component context-menu-mixin) class prototype value)
    (make-instance 'popup-menu-component
                   :target component
                   :content (icon show-context-menu)
                   :commands (make-context-menu-commands component class prototype value))))

(def (layered-function e) make-context-menu-commands (component class prototype value)
  (:method ((component component) class prototype value)
    nil)

  (:method ((component component) (class standard-class) (prototype standard-object) value)
    (append (call-next-method) (make-move-commands component class prototype value)))

  (:method ((component inspector-component) (class standard-class) (prototype standard-object) value)
    (optional-list* (make-refresh-command component class prototype value) (call-next-method)))

  (:method ((component inspector-component) (class built-in-class) (prototype null) value)
    nil))

;;;;;;
;;; Command bar mixin

(def component command-bar-mixin ()
  ((command-bar :type component))
  (:documentation "A component with a command bar"))

(def refresh command-bar-mixin
  (bind ((class (component-dispatch-class -self-))
         (prototype (when class (class-prototype class))))
    (setf (command-bar-of -self-) (make-command-bar -self- class prototype (component-value-of -self-)))))

(def (layered-function e) make-command-bar (component class prototype value)
  (:method ((component command-bar-mixin) class prototype value)
    (make-instance 'command-bar-component :commands (make-command-bar-commands component class prototype value))))

(def (layered-function e) make-command-bar-commands (component class prototype value)
  (:method ((component component) class prototype value)
    nil)

  (:method ((component inspector-component) (class standard-class) (prototype standard-object) value)
    (optional-list* (make-refresh-command component class prototype value) (call-next-method)))

  (:method ((component inspector-component) (class built-in-class) (prototype null) value)
    nil))

;;;;;;
;;; Commands mixin

(def component commands-mixin (context-menu-mixin command-bar-mixin)
  ())

(def (layered-function e) find-command (component name)
  (:method ((self commands-mixin) name)
    (bind (((:slots context-menu command-bar) self)
           (key [name-of (content-of !1)]))
      (or (find name (commands-of command-bar) :key key)
          (find name (commands-of context-menu) :key key)))))

;;;;;;
;;; Navigation bar component

(def component page-navigation-bar-component (command-bar-component)
  ((position 0)
   (page-size 10)
   (total-count)
   (first-command :type component)
   (previous-command :type component)
   (next-command :type component)
   (last-command :type component)
   (jumper :type component)
   (page-size-selector :type component)))

(def constructor page-navigation-bar-component ()
  (bind (((:slots position page-size total-count first-command previous-command next-command last-command jumper page-size-selector) -self-)
         (ajax (delay (id-of (parent-component-of -self-)))))
    (setf first-command (command (icon first)
                                 (make-action
                                   (setf (component-value-of jumper) (setf position 0)))
                                 :enabled (delay (> position 0))
                                 :ajax ajax)
          previous-command (command (icon previous)
                                    (make-action
                                      (setf (component-value-of jumper) (decf position (min position page-size))))
                                    :enabled (delay (> position 0))
                                    :ajax ajax)
          next-command (command (icon next)
                                (make-action
                                  (setf (component-value-of jumper) (incf position (min page-size (- total-count page-size)))))
                                :enabled (delay (< position (- total-count page-size)))
                                :ajax ajax)
          last-command (command (icon last)
                                (make-action
                                  (setf (component-value-of jumper) (setf position (- total-count page-size))))
                                :enabled (delay (< position (- total-count page-size)))
                                :ajax ajax)
          jumper (make-instance 'integer-inspector :edited #t :component-value position)
          page-size-selector (make-instance 'page-size-selector :component-value page-size))))

(def render-xhtml page-navigation-bar-component
  (bind (((:read-only-slots first-command previous-command next-command last-command jumper page-size) -self-))
    ;; FIXME: the select field rendered for page-count does not work by some fucking dojo reason
    (render-horizontal-list (list first-command previous-command jumper #+nil page-size-selector next-command last-command))))

;;;;;;
;;; Page count selector

(def component page-size-selector (member-inspector)
  ()
  (:default-initargs
   :edited #t
   :possible-values '(10 20 50 100)
   :client-name-generator [concatenate-string (integer-to-string !2) #"page-size-selector.rows/page"]))

(def refresh page-size-selector
  (setf (page-size-of (parent-component-of -self-)) (component-value-of -self-)))

(def resources en
  (page-size-selector.rows/page " rows/page"))

(def resources hu
  (page-size-selector.rows/page " sor/oldal"))

;;;;;;
;;; Generic commands

(def function ajax-id (component)
  (if (typep component 'remote-identity-mixin)
      (delay (id-of component))
      #t))

(def (layered-function e) make-refresh-command (component class prototype value)
  (:method ((component component) (class standard-class) (prototype standard-object) value)
    (command (icon refresh)
             (make-component-action component
               (refresh-component component))
             :ajax (ajax-id component))))

(def (function e) make-replace-command (original-component replacement-component &rest replace-command-args)
  "The REPLACE command replaces ORIGINAL-COMPONENT with REPLACEMENT-COMPONENT"
  (assert (and original-component
               replacement-component))
  (apply #'make-instance 'command-component
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
  (prog1-bind command (apply #'make-instance 'command-component args)
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
  (apply #'make-instance 'command-component
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

(def (layered-function e) make-focus-command (component classs prototype value)
  (:documentation "The FOCUS command replaces the top level COMPONENT usually found under the FRAME with the given REPLACEMENT-COMPONENT")

  (:method ((component component) (class standard-class) (prototype standard-object) value)
    (bind ((original-component (delay (find-top-component-content component))))
      (make-replace-and-push-back-command original-component component
                                          (list :content (icon focus-in) :visible (delay (not (top-component-p component))))
                                          (list :content (icon focus-out))))))

(def (generic e) make-frame-component-with-content (application content))

(def (layered-function e) make-open-in-new-frame-command (component class prototype value)
  (:method ((component component) (class standard-class) (prototype standard-object) value)
    (command (icon open-in-new-frame)
             (make-action (execute-open-in-new-frame component))
             :js (lambda (href) `js(window.open ,href))
             :delayed-content #t)))

(def (layered-function e) execute-open-in-new-frame (component)
  (:method ((component component))
    (bind ((clone (clone-component component))
           (*frame* (make-new-frame *application* *session*)))
      (setf (component-value-of clone) (component-value-of component))
      (setf (id-of *frame*) (insert-with-new-random-hash-table-key (frame-id->frame-of *session*)
                                                                   *frame* +frame-id-length+))
      (register-frame *application* *session* *frame*)
      (setf (root-component-of *frame*) (make-frame-component-with-content *application* clone))
      (make-redirect-response-with-frame-id-decorated *frame*))))
