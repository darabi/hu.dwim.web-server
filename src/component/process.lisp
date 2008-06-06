;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Process

;; TODO: try to kill this variable!
(def special-variable *standard-process-component*)

(def component standard-process-component (content-component user-message-collector-component-mixin)
  ((form)
   (closure/cc nil)
   (answer-continuation nil)
   (command-bar :type component)))

(def (macro e) standard-process (&body forms)
  `(make-instance 'standard-process-component :form '(progn ,@forms)))

(def render standard-process-component ()
  (with-slots (form closure/cc answer-continuation content) -self-
    (unless content
      (setf answer-continuation
            (bind ((*standard-process-component* -self-))
              (unless closure/cc
                (setf closure/cc (cl-delico::make-closure/cc (cl-walker:walk-form `(lambda () ,form)))))
              (with-call/cc
                (funcall closure/cc)))))
    (unless (and content answer-continuation)
      (setf content (make-instance 'label-component :component-value "Process finished")))
    <div
     ,(render-user-messages -self-)
     ,(render content)>))

(def (macro e) make-standard-process-component (&rest args &key form &allow-other-keys)
  (remove-from-plistf args :form)
  `(make-instance 'standard-process-component :form ',form ,@args))

(def function clear-process-component (component)
  (setf (content-of component) (empty))
  (replace-answer-commands component nil))

(def function replace-answer-commands (component answer-commands)
  (bind ((command-bar (command-bar-of component)))
    (setf (commands-of command-bar)
          (append (remove-if (lambda (command)
                               (typep command 'answer-command-component))
                             (commands-of command-bar))
                  answer-commands))))

(def (macro e) call (component answer-commands)
  `(let/cc k
     (setf (content-of *standard-process-component*) ,component)
     (replace-answer-commands *standard-process-component* (ensure-list ,answer-commands))
     k))

(def (generic e) answer (component value)
  (:method ((component component) value)
    (answer (find-ancestor-component-with-type component 'standard-process-component) value))

  (:method ((component standard-process-component) value)
    (bind ((*standard-process-component* component))
      (setf (answer-continuation-of *standard-process-component*)
            (kall (answer-continuation-of *standard-process-component*) (force value))))))

;;;;;;
;;; Answer command

(def component answer-command-component (command-component)
  ((icon (icon answer :label "Answer"))
   (action)
   (value nil)))

(def constructor answer-command-component ()
  (with-slots (icon action value) -self-
    (setf action (make-action (answer -self- value)))))

(def (macro e) answer-command (icon &body forms)
  `(make-instance 'answer-command-component :icon ,icon :value (delay ,@forms)))

(def method cl-quasi-quote::collect-slots-for-syntax-node-emitting-form ((node answer-command-component))
  (remove 'action (call-next-method) :key #'slot-definition-name))

;;;;;;
;;; Persistent processs

;; TODO: factor out common parts with the above, move code to dwim

(def component persistent-process-component (standard-process-component)
  ((process)))

(def constructor persistent-process-component ()
  (setf (command-bar-of -self-) (command-bar
                                  (make-open-in-new-frame-command -self-)
                                  (make-top-command -self-)
                                  (make-refresh-command -self-)
                                  (make-cancel-persistent-process-command -self-)
                                  (make-pause-persistent-process-command -self-))))

(def render persistent-process-component ()
  (with-slots (process command-bar answer-continuation content) -self-
    (prc::revive-instance process)
    (when (typep content 'empty-component)
      (add-user-information -self- (process.message.report-process-state process)))
    <div
     ,(render-user-messages -self-)
     ,(render content)
     ,(render command-bar) >))

(def function roll-persistent-process (component thunk)
  (setf (content-of component) nil)
  (bind ((*standard-process-component* component)
         (process (process-of component)))
    (setf (answer-continuation-of component)
          (rdbms::with-transaction
            (prc::with-revived-instance process
              (bind ((dmm::*process* process))
                (funcall thunk process)))))
    (unless (content-of component)
      (clear-process-component component))))

(def macro dmm:show-to-subject (component answer-commands &optional subject)
  "Shows a user interface component to the given subject."
  (once-only (subject)
    `(dmm::show-and-wait ,component
                         ,answer-commands
                         (or (not ,subject)
                             (and (dmm::has-authenticated-session)
                                  (eq ,subject (dmm::current-effective-subject))))
                         (make-instance 'dmm::wait-for-subject
                                        :subject (or ,subject
                                                     (dmm::current-effective-subject))))))

(def macro dmm:show-to-subject-expression (component answer-commands &optional expression)
  "Shows a user interface component to any one of the subjects matching to the given expression"
  `(dmm::show-and-wait ,component
                       ,answer-commands
                       (and (dmm::has-authenticated-session)
                            (bind ((dmm::-authenticated-subject- (dmm::current-authenticated-subject))
                                   (dmm::-effective-subject- (dmm::current-effective-subject)))
                              ,expression))
                       (or (dmm::select-wait-for-expression :wait-for-subject #t :expression ',expression)
                           (dmm::make-wait-for-expression :wait-for-subject #t :expression ',expression))))

;; TODO: defun/cc?
(def macro dmm:show-and-wait (component answer-commands &optional (condition t) (wait-reason nil))
  `(progn
     (dmm::persistent-process-wait dmm::*process* ,wait-reason)
     (if ,condition
         (call ,component ,answer-commands)
         (let/cc k
           (add-user-information *standard-process-component* #"process.message.waiting-for-other-subject")
           k))
     (assert (dmm::persistent-process-running-p dmm::*process*))))

(def method answer ((component persistent-process-component) value)
  (roll-persistent-process component
                           (lambda (process)
                             (dmm::process-event process 'dmm::process-state 'dmm::continue)
                             (kall (answer-continuation-of *standard-process-component*) (force value)))))

;;;;;;
;;; Command

(def icon start-process "static/wui/icons/20x20/vcr-play.png")
(defresources hu
  (icon-label.start-process "Elindítás")
  (icon-tooltip.start-process "A folyamat elindítása"))
(defresources en
  (icon-label.start-process "Start")
  (icon-tooltip.start-process "Start the process"))

(def icon continue-process "static/wui/icons/20x20/vcr-play.png")
(defresources hu
  (icon-label.continue-process "Folytatás")
  (icon-tooltip.continue-process "A folyamat folytatása"))
(defresources en
  (icon-label.continue-process "Continue")
  (icon-tooltip.continue-process "Continue the process"))

(def icon cancel-process "static/wui/icons/20x20/red-x.png")
(defresources hu
  (icon-label.cancel-process "Elvetés")
  (icon-tooltip.cancel-process "A folyamat elvetése"))
(defresources en
  (icon-label.cancel-process "Cancel")
  (icon-tooltip.cancel-process "Cancel the process"))

(def icon pause-process "static/wui/icons/20x20/stop-sign.png")
(defresources hu
  (icon-label.pause-process "Felfüggesztés")
  (icon-tooltip.pause-process "A folyamat felüggesztése"))
(defresources en
  (icon-label.pause-process "Pause")
  (icon-tooltip.pause-process "Pause the process"))

(def method make-standard-object-commands ((component standard-object-component) (class dmm::persistent-process) (instance prc::persistent-object))
  ;; TODO: move prc::with-revived-instance?
  (prc::with-revived-instance instance
    (optional-list (make-new-instance-command component)
                   (make-delete-instance-command component)
                   (when (dmm::persistent-process-initializing-p instance)
                     (make-start-persistent-process-command component instance))
                   (when (dmm::persistent-process-in-progress-p instance)
                     (make-continue-persistent-process-command component instance)))))

(def method make-standard-object-maker-commands ((component standard-object-maker-component) (class dmm::persistent-process))
  (list (make-start-persistent-process-command component (delay (execute-maker component (the-class-of component))))))

(def method make-standard-object-row-commands ((component standard-object-row-component) (class dmm::persistent-process) (instance prc::persistent-object))
  (prc::with-revived-instance instance
    (optional-list (make-expand-row-command component instance)
                   (when (dmm::persistent-process-initializing-p instance)
                     (make-start-persistent-process-command component instance))
                   (when (dmm::persistent-process-in-progress-p instance)
                     (make-continue-persistent-process-command component instance
                                                               (lambda (process-component)
                                                                 (make-instance 'entire-row-component :content process-component)))))))

(def (function e) make-start-persistent-process-command (component process)
  (make-replace-and-push-back-command component (delay (prog1-bind process-component (make-instance 'persistent-process-component :process (force process))
                                                         (roll-persistent-process process-component
                                                                                  (lambda (process)
                                                                                    (nth-value 1 (dmm::start-persistent-process process))))))
                                      (list :icon (icon start-process))
                                      (list :icon (icon back))))

(def (function e) make-continue-persistent-process-command (component process &optional (wrapper-thunk #'identity))
  (make-replace-and-push-back-command component (delay (bind ((process-component (make-instance 'persistent-process-component :process process)))
                                                         (roll-persistent-process process-component
                                                                                  (lambda (process)
                                                                                    (nth-value 1 (dmm::continue-persistent-process process))))
                                                         (funcall wrapper-thunk process-component)))
                                      (list :icon (icon continue-process) :visible (delay (prc::revive-instance process)
                                                                                          (dmm::persistent-process-in-progress-p process)))
                                      (list :icon (icon back))))

(def (function e) make-cancel-persistent-process-command (component)
  (command (icon cancel-process)
           (make-action (rdbms::with-transaction
                          (prc::revive-instance (process-of component))
                          (dmm::cancel-persistent-process (process-of component))
                          (clear-process-component component)))
           :visible (delay (or (dmm::persistent-process-paused-p (process-of component))
                               (dmm::persistent-process-in-progress-p (process-of component))))))

(def (function e) make-pause-persistent-process-command (component)
  (command (icon pause-process)
           (make-action (rdbms::with-transaction
                          (prc::revive-instance (process-of component))
                          (dmm::pause-persistent-process (process-of component))
                          (clear-process-component component)))
           :visible (delay (or (dmm::persistent-process-paused-p (process-of component))
                               (dmm::persistent-process-in-progress-p (process-of component))))))

;;;;;;
;;; Localization

(defresources hu
  (process.message.waiting-for-other-subject "A folyamat jelenleg másra várakozik.")
  (process.message.waiting "A folyamat jelenleg várakozik.")
  (process.message.report-process-state (process)
    (ecase (dmm::element-name-of (dmm::process-state-of process))
      ;; a process in 'running state may not reach this point
      (dmm::finished    "Folyamat normálisan befejeződött")
      (dmm::failed      "Folyamat hibára futott")
      (dmm::broken      "Folyamat technikai hiba miatt megállítva")
      (dmm::cancelled   "Folyamat felhasználó által leállítva")
      (dmm::in-progress "Folyamat folyamatban")
      (dmm::paused      "Folyamat félbeszakítva"))))

(defresources en
  (process.message.waiting-for-other-subject "Process is waiting for other subject.")
  (process.message.waiting "Process is currently waiting.")
  (process.message.report-process-state (process)
    (ecase (dmm::element-name-of (dmm::process-state-of process))
      ;; a process in 'running state may not reach this point
      (dmm::finished    "Process finished normally")
      (dmm::failed      "Process failed")
      (dmm::broken      "Process was stopped due to technical failures")
      (dmm::cancelled   "Process has been cancelled")
      (dmm::in-progress "Process is in progress")
      (dmm::paused      "Process is paused"))))
