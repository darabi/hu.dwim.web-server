;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Persistent processs

(def (component ea) persistent-process-component (standard-process-component)
  ((process)))

(def layered-method make-context-menu-items ((component persistent-process-component) (class dmm::persistent-process) (prototype dmm::standard-persistent-process) (instance dmm::standard-persistent-process))
  (append (call-next-method)
          (optional-list (make-cancel-persistent-process-command component)
                         (make-pause-persistent-process-command component))))

(def render-xhtml persistent-process-component ()
  (bind (((:slots process command-bar answer-continuation content)) -self-)
    (prc::revive-instance process)
    (when (typep content 'empty-component)
      (add-user-information -self- (process.message.report-process-state process)))
    <div ,(render-user-messages -self-)
         ,(render-component content)
         ,(render-component command-bar) >))

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

(def function/cc dmm:show-to-subject (subject component &key answer-commands)
  "Shows a user interface component to the given subject."
  (dmm:show-maybe component
                  :answer-commands answer-commands
                  :when (or (not subject)
                            (and (dmm::has-authenticated-session)
                                 (prc:p-eq subject (dmm::current-effective-subject))))
                  :wait-reason (make-instance 'dmm::wait-for-subject
                                              :subject (or subject
                                                           (dmm::current-effective-subject)))))

(def function/cc dmm:show-to-current-effective-subject (component &key answer-commands)
  (dmm:show-to-subject (dmm:current-effective-subject) component :answer-commands answer-commands))

(def macro dmm:show-to-subjects-matching-expression (expression component answer-commands)
  "Shows a user interface component to any one of the subjects matching to the given expression"
  `(dmm:show-maybe ,component
                   :answer-commands ,answer-commands
                   :when (and (dmm::has-authenticated-session)
                              (bind ((dmm::-authenticated-subject- (dmm::current-authenticated-subject))
                                     (dmm::-effective-subject- (dmm::current-effective-subject)))
                                ,expression))
                   :wait-reason (or (dmm::select-wait-for-expression :wait-for-subject #t :expression ',expression)
                                    (dmm::make-wait-for-expression :wait-for-subject #t :expression ',expression))))

(def function/cc dmm:show-maybe (component &key answer-commands (when t) (wait-reason nil))
  (dmm::persistent-process-wait dmm::*process* wait-reason)
  (if when
      (call-component component :answer-commands answer-commands)
      (let/cc k
        (add-user-information *standard-process-component* #"process.message.waiting-for-other-subject")
        k))
  (assert (dmm::persistent-process-running-p dmm::*process*))
  (values))

(def method answer-component ((component persistent-process-component) value)
  (roll-persistent-process component
                           (lambda (process)
                             (dmm::process-event process 'dmm::process-state 'dmm::continue)
                             (kall (answer-continuation-of *standard-process-component*) (force value)))))

;;;;;;
;;; Command

(def icon start-process)

(def icon continue-process)

(def icon cancel-process)

(def icon pause-process)

(def layered-method make-context-menu-items ((component standard-object-inspector) (class dmm::persistent-process) (prototype dmm::standard-persistent-process) (instance dmm::standard-persistent-process))
  ;; TODO: move prc::with-revived-instance?
  (prc::with-revived-instance instance
    (optional-list #+nil
                   (make-new-instance-command component)
                   (make-delete-instance-command component class instance)
                   (when (dmm::persistent-process-initializing-p instance)
                     (make-start-persistent-process-command component instance))
                   (when (dmm::persistent-process-in-progress-p instance)
                     (make-continue-persistent-process-command component instance)))))

(def layered-method make-context-menu-items ((component standard-object-maker) (class dmm::persistent-process) (prototype dmm::standard-persistent-process) (instance dmm::standard-persistent-process))
  (list (make-start-persistent-process-command component
                                               (delay (create-instance component (the-class-of component))))))

(def layered-method make-context-menu-items ((component standard-object-row-inspector) (class dmm::persistent-process) (prototype dmm::standard-persistent-process) (instance dmm::standard-persistent-process))
  (prc::with-revived-instance instance
    (optional-list (make-expand-command component class prototype instance)
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
                                      (list :content (icon start-process))
                                      (list :content (icon back))))

(def (function e) make-continue-persistent-process-command (component process &optional (wrapper-thunk #'identity))
  (make-replace-and-push-back-command component (delay (bind ((process-component (make-instance 'persistent-process-component :process process)))
                                                         (roll-persistent-process process-component
                                                                                  (lambda (process)
                                                                                    (nth-value 1 (dmm::continue-persistent-process process))))
                                                         (funcall wrapper-thunk process-component)))
                                      (list :content (icon continue-process) :visible (delay (prc::revive-instance process)
                                                                                             (dmm::persistent-process-in-progress-p process)))
                                      (list :content (icon back))))

(def (function e) make-cancel-persistent-process-command (component)
  (command (icon cancel-process)
           (make-component-action component
             (rdbms::with-transaction
               (prc::revive-instance (process-of component))
               (dmm::cancel-persistent-process (process-of component))
               (clear-process-component component)))
           :visible (delay (or (dmm::persistent-process-paused-p (process-of component))
                               (dmm::persistent-process-in-progress-p (process-of component))))))

(def (function e) make-pause-persistent-process-command (component)
  (command (icon pause-process)
           (make-component-action component
             (rdbms::with-transaction
               (prc::revive-instance (process-of component))
               (dmm::pause-persistent-process (process-of component))
               (clear-process-component component)))
           :visible (delay (or (dmm::persistent-process-paused-p (process-of component))
                               (dmm::persistent-process-in-progress-p (process-of component))))))
