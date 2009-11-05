;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Persistent processs

(def (component e) persistent-process-component (standard-process-component)
  ((process)))

(def layered-method make-context-menu-items ((component persistent-process-component) (class hu.dwim.meta-model::persistent-process) (prototype hu.dwim.meta-model::standard-persistent-process) (instance hu.dwim.meta-model::standard-persistent-process))
  (append (call-next-method)
          (optional-list (make-cancel-persistent-process-command component)
                         (make-pause-persistent-process-command component))))

(def render-xhtml persistent-process-component ()
  (bind (((:slots process command-bar answer-continuation content) -self-))
    (hu.dwim.perec::revive-instance process)
    (when (empty-layout? content)
      (add-component-information-message -self- (process.message.report-process-state process)))
    <div ,(render-component-messages-for -self-)
         ,(render-component content)
         ,(render-component command-bar) >))

(def function roll-persistent-process (component thunk)
  (setf (content-of component) nil)
  (bind ((*standard-process-component* component)
         (process (component-value-of component)))
    (setf (answer-continuation-of component)
          (hu.dwim.rdbms::with-transaction
            (hu.dwim.perec::with-revived-instance process
              (bind ((hu.dwim.meta-model::*process* process))
                (funcall thunk process)))))
    (unless (content-of component)
      (clear-process-component component))))

(def function/cc hu.dwim.meta-model:show-to-subject (subject component &key answer-commands)
  "Shows a user interface component to the given subject."
  (hu.dwim.meta-model:show-maybe component
                                 :answer-commands answer-commands
                                 :when (or (not subject)
                                           (and (hu.dwim.meta-model::has-authenticated-session)
                                                (hu.dwim.perec:p-eq subject (hu.dwim.meta-model::current-effective-subject))))
                                 :wait-reason (make-instance 'hu.dwim.meta-model::wait-for-subject
                                                             :subject (or subject
                                                                          (hu.dwim.meta-model::current-effective-subject)))))

(def function/cc hu.dwim.meta-model:show-to-current-effective-subject (component &key answer-commands)
  (hu.dwim.meta-model:show-to-subject (hu.dwim.meta-model:current-effective-subject) component :answer-commands answer-commands))

(def macro hu.dwim.meta-model:show-to-subjects-matching-expression (expression component answer-commands)
  "Shows a user interface component to any one of the subjects matching to the given expression"
  `(hu.dwim.meta-model:show-maybe ,component
                                  :answer-commands ,answer-commands
                                  :when (and (hu.dwim.meta-model::has-authenticated-session)
                                             (bind ((hu.dwim.meta-model::-authenticated-subject- (hu.dwim.meta-model::current-authenticated-subject))
                                                    (hu.dwim.meta-model::-effective-subject- (hu.dwim.meta-model::current-effective-subject)))
                                               ,expression))
                                  :wait-reason (or (hu.dwim.meta-model::select-wait-for-expression :wait-for-subject #t :expression ',expression)
                                                   (hu.dwim.meta-model::make-wait-for-expression :wait-for-subject #t :expression ',expression))))

(def function/cc hu.dwim.meta-model:show-maybe (component &key answer-commands (when t) (wait-reason nil))
  (hu.dwim.meta-model::persistent-process-wait hu.dwim.meta-model::*process* wait-reason)
  (if when
      (call-component component :answer-commands answer-commands)
      (let/cc k
        (add-user-information *standard-process-component* #"process.message.waiting-for-other-subject")
        k))
  (assert (hu.dwim.meta-model::persistent-process-running-p hu.dwim.meta-model::*process*))
  (values))

(def method answer-component ((component persistent-process-component) value)
  (roll-persistent-process component
                           (lambda (process)
                             (hu.dwim.meta-model::process-event process 'hu.dwim.meta-model::process-state 'hu.dwim.meta-model::continue)
                             (kall (answer-continuation-of *standard-process-component*) (force value)))))

;;;;;;
;;; Command

(def icon start-process)

(def icon continue-process)

(def icon cancel-process)

(def icon pause-process)

(def function make-persistent-process-commands (component instance)
  ;; TODO: move hu.dwim.perec::with-revived-instance?
  (hu.dwim.perec::with-revived-instance instance
    (optional-list #+nil
                   (make-new-instance-command component)
                   #+nil
                   (make-delete-instance-command component class instance)
                   (when (hu.dwim.meta-model::persistent-process-initializing-p instance)
                     (make-start-persistent-process-command component instance))
                   (when (hu.dwim.meta-model::persistent-process-in-progress-p instance)
                     (make-continue-persistent-process-command component instance)))))

(def layered-method make-context-menu-items ((component t/inspector) (class hu.dwim.meta-model::persistent-process) (prototype hu.dwim.meta-model::standard-persistent-process) (instance hu.dwim.meta-model::standard-persistent-process))
  (append (make-persistent-process-commands component instance)
          (call-next-method)))

(def layered-method make-command-bar-commands ((component t/inspector) (class hu.dwim.meta-model::persistent-process) (prototype hu.dwim.meta-model::standard-persistent-process) (instance hu.dwim.meta-model::standard-persistent-process))
  (append (make-persistent-process-commands component instance)
          (call-next-method)))

#+nil
(def layered-method make-context-menu-items ((component t/maker) (class hu.dwim.meta-model::persistent-process) (prototype hu.dwim.meta-model::standard-persistent-process) (instance hu.dwim.meta-model::standard-persistent-process))
  (list (make-start-persistent-process-command component
                                               (delay (create-instance component (the-class-of component))))))

#+nil
(def layered-method make-context-menu-items ((component standard-object-row-inspector) (class hu.dwim.meta-model::persistent-process) (prototype hu.dwim.meta-model::standard-persistent-process) (instance hu.dwim.meta-model::standard-persistent-process))
  (hu.dwim.perec::with-revived-instance instance
    (optional-list (make-expand-command component class prototype instance)
                   (when (hu.dwim.meta-model::persistent-process-initializing-p instance)
                     (make-start-persistent-process-command component instance))
                   (when (hu.dwim.meta-model::persistent-process-in-progress-p instance)
                     (make-continue-persistent-process-command component instance
                                                               (lambda (process-component)
                                                                 (make-instance 'entire-row-component :content process-component)))))))

(def (function e) make-start-persistent-process-command (component process)
  (make-replace-and-push-back-command component (delay (prog1-bind process-component (make-instance 'persistent-process-component :component-value (force process))
                                                         (roll-persistent-process process-component
                                                                                  (lambda (process)
                                                                                    (nth-value 1 (hu.dwim.meta-model::start-persistent-process process))))))
                                      (list :content (icon start-process))
                                      (list :content (icon navigate-back))))

(def (function e) make-continue-persistent-process-command (component process &optional (wrapper-thunk #'identity))
  (make-replace-and-push-back-command component (delay (bind ((process-component (make-instance 'persistent-process-component :component-value process)))
                                                         (roll-persistent-process process-component
                                                                                  (lambda (process)
                                                                                    (nth-value 1 (hu.dwim.meta-model::continue-persistent-process process))))
                                                         (funcall wrapper-thunk process-component)))
                                      (list :content (icon continue-process) :visible (delay (hu.dwim.perec::revive-instance process)
                                                                                             (hu.dwim.meta-model::persistent-process-in-progress-p process)))
                                      (list :content (icon navigate-back))))

(def (function e) make-cancel-persistent-process-command (component)
  (command/widget (:visible (delay (or (hu.dwim.meta-model::persistent-process-paused-p (component-value-of component))
                                       (hu.dwim.meta-model::persistent-process-in-progress-p (component-value-of component)))))
    (icon cancel-process)
    (make-component-action component
      (hu.dwim.rdbms::with-transaction
        (hu.dwim.perec::revive-instance (component-value-of component))
        (hu.dwim.meta-model::cancel-persistent-process (component-value-of component))
        (clear-process-component component)))))

(def (function e) make-pause-persistent-process-command (component)
  (command/widget (:visible (delay (or (hu.dwim.meta-model::persistent-process-paused-p (component-value-of component))
                                       (hu.dwim.meta-model::persistent-process-in-progress-p (component-value-of component)))))
    (icon pause-process)
    (make-component-action component
      (hu.dwim.rdbms::with-transaction
        (hu.dwim.perec::revive-instance (component-value-of component))
        (hu.dwim.meta-model::pause-persistent-process (component-value-of component))
        (clear-process-component component)))))
