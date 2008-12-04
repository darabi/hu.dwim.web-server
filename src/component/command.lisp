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

(def component command-component (content-component)
  ((action :type (or uri action))
   (enabled #t :type boolean)
   ;; TODO: put a lambda with the authorization rule captured here in cl-perec integration
   ;; TODO: always wrap the action lambda with a call to execute-command
   (available #t :type boolean)
   (default #f :type boolean)
   (js nil)
   (action-arguments nil)))

(def (macro e) command (content action &key (enabled #t) (visible #t) (default #f) js scheme path
                                (delayed-content nil delayed-content-provided?)
                                (send-client-state #t send-client-state-provided?))
  `(make-instance 'command-component
                  :content ,content
                  :action ,action
                  :enabled ,enabled
                  :visible ,visible
                  :default ,default
                  :js ,js
                  :action-arguments (list :delayed-content ,delayed-content
                                          :scheme ,scheme
                                          :path ,path
                                          :send-client-state ,send-client-state)))

(def render command-component ()
  (bind (((:read-only-slots content action enabled default js action-arguments) -self-))
    (render-command content action :enabled enabled :default default :ajax #f :js js :action-arguments action-arguments)))

(def render :in passive-components-layer command-component
  (render (content-of -self-)))

(def (function e) render-command (content action &key (enabled #t) (default #f) (ajax (not (null *frame*))) js action-arguments)
  (if (force enabled)
      (bind ((send-client-state (prog1
                                    (getf action-arguments :send-client-state #t)
                                  (remove-from-plistf action-arguments :send-client-state)))
             (href (etypecase action
                     (action (apply 'register-action/href action action-arguments))
                     ;; TODO wastes resources. store back the printed uri? see below also...
                     (uri (print-uri-to-string action))))
             (onclick-js (or js
                             (lambda (href)
                               `js-inline(return (wui.io.action ,href ,ajax ,send-client-state)))))
             (name (when (running-in-test-mode-p *application*)
                     (if (typep content 'icon-component)
                         (symbol-name (name-of content))
                         (princ-to-string content)))))
        <a (:href "#" :onclick ,(funcall onclick-js href) :name ,name)
           ,(render content)>
        (when default
          <input (:type "submit" :style "display: none;" :onclick ,(funcall onclick-js href))>))
      (render content)))

(def layered-method render-onclick-handler ((command command-component))
  (bind ((action (action-of command))
         (href (etypecase action
                 (action (register-action/href action))
                 (uri (print-uri-to-string action)))))
    `js-inline(return (wui.io.action ,href #f #t))))

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
  `(make-instance 'command-bar-component :commands (list ,@commands)))

(def render command-bar-component
  (bind (((:read-only-slots parent-component commands) -self-)
         (sorted-commands (sort-commands parent-component commands)))
    (setf (commands-of -self-) sorted-commands)
    (render-horizontal-list sorted-commands :css-class "command-bar")))

(def render-csv command-bar-component ()
  (iter (for command :in (commands-of -self-))
        (unless (first-iteration-p)
          (write-char #\Space *csv-stream*))
        (render-csv command)))

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
                 (or (position (name-of (content-of command))
                               '(answer back focus-out open-in-new-frame focus-in collapse collapse-all expand-all refresh edit save cancel store revert new delete)
                               :test #'eq)
                     most-positive-fixnum)))))

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

(def (function e) find-command-bar-command (command-bar name)
  (find name (commands-of command-bar)
        :key (lambda (command)
               (name-of (content-of command)))))

(def (function e) execute-command-bar-command (command-bar name)
  (execute-command (find-command-bar-command command-bar name)))

;;;;;
;;; Popup command menu

(def component popup-command-menu-component (style-component-mixin remote-identity-component-mixin)
  ((commands nil :type components)))

(def render popup-command-menu-component ()
  (bind (((:read-only-slots commands id css-class style) -self-)
         (menu-id (generate-frame-unique-string)))
    (when commands
      <div (:id ,id :class ,css-class :style ,style)
          <img (:src "static/wui/icons/20x20/green-star.png")>
          ,(render-dojo-widget (menu-id)
             <div (:id ,menu-id
                   :dojoType #.+dijit/menu+
                   :targetNodeIds ,id
                   :style "display: none;")
               ,(iter (for command :in commands)
                      (for command-id = (generate-frame-unique-string))
                      (when (force (visible-p command))
                        (render-dojo-widget (command-id)
                          <div (:id ,command-id
                                :dojoType #.+dijit/menu-item+
                                :iconClass ,(icon-class (name-of (content-of command)))
                                :onclick ,(render-onclick-handler command))
                            ,(render-icon :icon (content-of command) :class nil)>)))>)>)))

(def render-csv popup-command-menu-component ()
  (iter (for command :in (commands-of -self-))
        (unless (first-iteration-p)
          (write-char #\Space *csv-stream*))
        (render-csv command)))

;;;;;;
;;; Navigation bar component

(def component page-navigation-bar-component (command-bar-component)
  ((position 0)
   (page-count 10)
   (total-count)
   (first-command :type component)
   (previous-command :type component)
   (next-command :type component)
   (last-command :type component)
   (jumper :type component)
   (page-count-selector :type component)))

(def constructor page-navigation-bar-component ()
  (with-slots (position page-count total-count first-command previous-command next-command last-command jumper page-count-selector) -self-
    (setf first-command (command (icon first)
                                 (make-action
                                   (setf (component-value-of jumper) (setf position 0)))
                                 :enabled (delay (> position 0)))
          previous-command (command (icon previous)
                                    (make-action
                                      (setf (component-value-of jumper) (decf position (min position page-count))))
                                    :enabled (delay (> position 0)))
          next-command (command (icon next)
                                (make-action
                                  (setf (component-value-of jumper) (incf position (min page-count (- total-count page-count)))))
                                :enabled (delay (< position (- total-count page-count))))
          last-command (command (icon last)
                                (make-action
                                  (setf (component-value-of jumper) (setf position (- total-count page-count))))
                                :enabled (delay (< position (- total-count page-count))))
          jumper (make-instance 'integer-inspector :edited #t :component-value position)
          page-count-selector (make-instance 'page-count-selector :component-value page-count))))

(def render page-navigation-bar-component ()
  (bind (((:read-only-slots first-command previous-command next-command last-command jumper page-count-selector) -self-))
    (render-horizontal-list (list first-command previous-command jumper page-count-selector next-command last-command))))

;;;;;;
;;; Page count selector

(def component page-count-selector (member-inspector)
  ()
  (:default-initargs :possible-values '(10 20 50 100) :edited #t :client-name-generator [princ-to-string !2]))

(def method refresh-component ((self page-count-selector))
  (setf (page-count-of (parent-component-of self)) (component-value-of self)))

;;;;;;
;;; Generic commands

(def (generic e) refresh-component (component)
  (:method ((self component))
    (values))

  (:method :after ((self component))
    (setf (outdated-p self) #f)))

(def (layered-function e) make-refresh-command (component class prototype-or-instance)
  (:method ((component inspector-component) (class standard-class) (prototype-or-instance standard-object))
    (command (icon refresh)
             (make-component-action component
               (refresh-component component)))))

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

(def (function e) find-top-component-content (component)
  (awhen (find-ancestor-component-with-type component 'top-component)
    (content-of it)))

(def (function e) top-component-p (component)
  (eq component (find-top-component-content component)))

(def (layered-function e) make-focus-command (component classs prototype-or-instance)
  (:documentation "The FOCUS command replaces the top level COMPONENT usually found under the FRAME with the given REPLACEMENT-COMPONENT")

  (:method ((component component) (class standard-class) (prototype-or-instance standard-object))
    (bind ((original-component (delay (find-top-component-content component))))
      (make-replace-and-push-back-command original-component component
                                          (list :content (icon focus-in) :visible (delay (not (top-component-p component))))
                                          (list :content (icon focus-out))))))

(def (generic e) make-frame-component-with-content (application content))

(def (layered-function e) make-open-in-new-frame-command (component class prototype-or-instance)
  (:method ((component component) (class standard-class) (prototype-or-instance standard-object))
    (command (icon open-in-new-frame)
             (make-action
               (bind ((clone (clone-component component))
                      (*frame* (make-new-frame *application* *session*)))
                 (setf (component-value-of clone) (component-value-of component))
                 (setf (id-of *frame*) (insert-with-new-random-hash-table-key (frame-id->frame-of *session*)
                                                                              *frame* +frame-id-length+))
                 (register-frame *application* *session* *frame*)
                 (setf (root-component-of *frame*) (make-frame-component-with-content *application* clone))
                 (make-redirect-response-with-frame-id-decorated *frame*)))
             :js (lambda (href)
                   `js-inline(window.open ,href))
             :delayed-content #t)))
