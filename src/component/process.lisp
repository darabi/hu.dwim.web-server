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
   (answer-continuation nil)))

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

(def (macro e) call (component)
  `(let/cc k
     (setf (content-of *standard-process-component*) ,component)
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
  ((icon (make-icon-component 'answer :label "Answer"))
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

(def render persistent-process-component ()
  (with-slots (process answer-continuation content) -self-
    (unless content
      (setf answer-continuation
            (bind ((*standard-process-component* -self-))
              (nth-value 1 (rdbms::with-transaction
                             (prc::revive-instance process)
                             (dmm::with-new-persistent-process-context (:renderer -self- :process process) ;; TODO: rename renderer to component
                               (if (dmm::persistent-process-in-progress-p process)
                                   (dmm::continue-persistent-process-in-context)
                                   (dmm::start-persistent-process-in-context))))))))
    (unless (and content answer-continuation)
      (setf content (make-instance 'label-component :component-value "Process finished")))
    (call-next-method)))

(def macro dmm:show-to-subject (component &optional subject)
  "Shows a user interface component to the given subject."
  (once-only (subject)
    `(dmm::show-and-wait ,component
                         (or (not ,subject)
                             (and (dmm::has-authenticated-session)
                                  (eq ,subject (dmm::current-effective-subject))))
                         (make-instance 'dmm::wait-for-subject
                                        :subject (or ,subject
                                                     (dmm::current-effective-subject))))))

(def macro dmm:show-to-subject-expression (component &optional expression)
  "Shows a user interface component to any one of the subjects matching to the given expression"
  `(dmm::show-and-wait ,component
                       (and (dmm::has-authenticated-session)
                            (bind ((dmm::-authenticated-subject- (dmm::current-authenticated-subject))
                                   (dmm::-effective-subject- (dmm::current-effective-subject)))
                              ,expression))
                       (or (dmm::select-wait-for-expression :wait-for-subject #t :expression ',expression)
                           (dmm::make-wait-for-expression :wait-for-subject #t :expression ',expression))))

;; TODO: defun/cc?
(def macro dmm:show-and-wait (component &optional (condition t) (wait-reason nil))
  `(progn
     (dmm::persistent-process-wait dmm::*process* ,wait-reason)
     (call (if ,condition
               ,component
               (progn
                 (add-user-information *standard-process-component* #"process.message.waiting-for-other-subject")
                 (empty))))
     (assert (eq (dmm::element-name-of (dmm::process-state-of dmm::*process*)) 'dmm::running))))

(def method answer ((component persistent-process-component) value)
  (bind ((*standard-process-component* component))
    (rdbms::with-transaction
      (prc::revive-instance (process-of component))
      (dmm::with-new-persistent-process-context (:renderer component :process (process-of component))
        (dmm::process-event (process-of component) 'dmm::process-state 'dmm::continue)
        (setf (answer-continuation-of *standard-process-component*)
              (kall (answer-continuation-of *standard-process-component*) (force value)))))))

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

(def method make-standard-object-commands ((component standard-object-component) (class dmm::persistent-process) (instance prc::persistent-object))
  (optional-list (make-new-instance-command component)
                 (make-delete-instance-command component)
                 (when (dmm::persistent-process-initializing-p instance)
                   (make-start-persistent-process-command component instance))
                 (when (dmm::persistent-process-in-progress-p instance)
                   (make-continue-persistent-process-command component instance))))

(def method make-standard-object-maker-commands ((component standard-object-maker-component) (class dmm::persistent-process))
  (list (make-start-persistent-process-command component (delay (execute-maker component (the-class-of component))))))

(def method make-standard-object-row-commands ((component standard-object-row-component) (class dmm::persistent-process) (instance prc::persistent-object))
  (optional-list (make-expand-row-command component instance)
                 (when (dmm::persistent-process-initializing-p instance)
                   (make-start-persistent-process-command component instance))
                 (when (dmm::persistent-process-in-progress-p instance)
                   (make-continue-persistent-process-command component instance
                                                             (lambda (process-component)
                                                               (make-instance 'entire-row-component :content process-component))))))

(def (function e) make-start-persistent-process-command (component instance)
  (make-replace-command component (delay (make-instance 'persistent-process-component :process (force instance)))
                        :icon (clone-icon 'start-process)))

(def (function e) make-continue-persistent-process-command (component instance &optional (wrapper-thunk #'identity))
  (make-replace-command component (delay (funcall wrapper-thunk (make-instance 'persistent-process-component :process instance)))
                        :icon (clone-icon 'continue-process)))
;;;;;;
;;; Localization

(defresources hu
  (process.message.waiting-for-other-subject "A folyamat jelenleg máshoz tartozik.")
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
