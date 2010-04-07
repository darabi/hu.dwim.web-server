;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/inspector

(def layered-method make-alternatives ((component t/inspector) (class hu.dwim.meta-model::persistent-process-class) (prototype hu.dwim.meta-model::persistent-process) (value hu.dwim.meta-model::persistent-process))
  (list* (make-instance 'standard-process/user-interface/inspector
                        :component-value value
                        :component-value-type (component-value-type-of component))
         (call-next-method)))

;;;;;;
;;; Show

(def (function/cc e) show-to-subject (subject component &key answer-commands)
  "Shows a user interface component to the given subject."
  (show-maybe component
              :answer-commands answer-commands
              :when (or (not subject)
                        (and (hu.dwim.meta-model::has-authenticated-session?)
                             (hu.dwim.perec:p-eq subject (hu.dwim.meta-model::current-effective-subject))))
              :wait-reason (when subject
                             (make-instance 'hu.dwim.meta-model::wait-for-subject
                                            :subject (or subject
                                                         (hu.dwim.meta-model::current-effective-subject))))))

(def (function/cc e) show-to-current-effective-subject (component &key answer-commands)
  (show-to-subject (hu.dwim.meta-model:current-effective-subject) component :answer-commands answer-commands))

(def (macro e) show-to-subjects-matching-expression (expression component answer-commands)
  "Shows a user interface component to any one of the subjects matching to the given expression"
  `(show-maybe ,component
               :answer-commands ,answer-commands
               :when (and (hu.dwim.meta-model::has-authenticated-session?)
                          (bind ((hu.dwim.meta-model::-authenticated-subject- (hu.dwim.meta-model::current-authenticated-subject))
                                 (hu.dwim.meta-model::-effective-subject- (hu.dwim.meta-model::current-effective-subject)))
                            ,expression))
               :wait-reason (or (hu.dwim.meta-model::select-wait-for-expression :wait-for-subject #t :expression ',expression)
                                (hu.dwim.meta-model::make-wait-for-expression :wait-for-subject #t :expression ',expression))))

(def (function/cc e) show-maybe (component &key answer-commands (when #t) (wait-reason nil))
  (hu.dwim.meta-model::persistent-process-wait *process* wait-reason)
  (prog1
      (if when
          (call-component component answer-commands)
          (let/cc k
            (add-component-information-message *process-component* #"process.message.waiting-for-other-subject")
            k))
    (assert (process-running? *process*))))

;;;;;;
;;; Persistent processs

(def layered-method make-context-menu-items ((component standard-process/user-interface/inspector) (class hu.dwim.meta-model::persistent-process-class) (prototype hu.dwim.meta-model::persistent-process) (value hu.dwim.meta-model::persistent-process))
  (append (call-next-method)
          (optional-list (make-cancel-persistent-process-command component)
                         (make-pause-persistent-process-command component))))

(def layered-method roll-process ((component standard-process/user-interface/inspector) (class hu.dwim.meta-model::persistent-process-class) (prototype hu.dwim.meta-model::persistent-process) (value hu.dwim.meta-model::persistent-process) thunk)
  (hu.dwim.rdbms::with-transaction
    (hu.dwim.perec::with-reloaded-instance value
      (bind ((*process* value))
        (call-next-layered-method component class prototype value thunk)))))

;; TODO: reuse and kill
#+nil
(when (empty-layout? content)
  (add-component-information-message -self- (process.message.report-process-state component-value)))

;; TODO: reuse and kill
#+nil
(when (process-in-stop-state? process)
  (finish-process-component component))

;; TODO: just kill
#+nil
(def method answer-component ((component persistent-process/user-interface/inspector) value)
  (roll-process component
                (lambda (process)
                  (hu.dwim.meta-model::process-event process 'hu.dwim.meta-model::process-state 'hu.dwim.meta-model::continue)
                  (kall (answer-continuation-of *process-component*) (force value)))))

;;;;;;
;;; Persistent process specific commands

(def icon start-process)

(def icon continue-process)

(def icon cancel-process)

(def icon pause-process)

(def (layered-function e) make-persistent-process-commands (component class prototype value)
  ;; TODO: move hu.dwim.perec::with-revived-instance?
  (:method ((component t/inspector) class prototype value)
    (hu.dwim.perec::with-revived-instance value
      (optional-list (when (process-initializing? value)
                       (make-start-persistent-process-command component value))
                     (when (process-in-progress? value)
                       (make-continue-persistent-process-command component value))))))

(def layered-method make-context-menu-items ((component t/inspector) (class hu.dwim.meta-model::persistent-process-class) (prototype hu.dwim.meta-model::persistent-process) (value hu.dwim.meta-model::persistent-process))
  (append (make-persistent-process-commands component class prototype value) (call-next-method)))

(def layered-method make-command-bar-commands ((component t/inspector) (class hu.dwim.meta-model::persistent-process-class) (prototype hu.dwim.meta-model::persistent-process) (value hu.dwim.meta-model::persistent-process))
  (append (make-persistent-process-commands component class prototype value) (call-next-method)))

(def (function e) make-start-persistent-process-command (component process)
  (make-replace-and-push-back-command component (delay (prog1-bind process-component (make-instance 'standard-process/user-interface/inspector :component-value (force process))
                                                         (roll-process process-component (component-dispatch-class process-component) (component-dispatch-prototype process-component) process
                                                                       (lambda (process)
                                                                         (nth-value 1 (hu.dwim.meta-model::start-persistent-process process))))))
                                      (list :content (icon/widget start-process))
                                      (list :content (icon/widget navigate-back))))

(def (function e) make-continue-persistent-process-command (component process)
  (make-replace-and-push-back-command component (delay (prog1-bind process-component (make-instance 'standard-process/user-interface/inspector :component-value process)
                                                         (roll-process process-component (component-dispatch-class process-component) (component-dispatch-prototype process-component) process
                                                                       (lambda (process)
                                                                         (nth-value 1 (hu.dwim.meta-model::continue-persistent-process process))))))
                                      (list :content (icon/widget continue-process) :visible (delay (hu.dwim.perec::revive-instance process)
                                                                                             (process-in-progress? process)))
                                      (list :content (icon/widget navigate-back))))

(def (function e) make-cancel-persistent-process-command (component)
  (command/widget (:visible (delay (or (process-paused? (component-value-of component))
                                       (process-in-progress? (component-value-of component)))))
    (icon/widget cancel-process)
    (make-component-action component
      (hu.dwim.rdbms::with-transaction
        (hu.dwim.perec::revive-instance (component-value-of component))
        (cancel-process (component-value-of component))
        ;; TODO:
        #+nil
        (finish-process-component component)))))

(def (function e) make-pause-persistent-process-command (component)
  (command/widget (:visible (delay (or (process-paused? (component-value-of component))
                                       (process-in-progress? (component-value-of component)))))
    (icon/widget pause-process)
    (make-component-action component
      (hu.dwim.rdbms::with-transaction
        (hu.dwim.perec::revive-instance (component-value-of component))
        (pause-process (component-value-of component))
        ;; TODO:
        #+nil
        (finish-process-component component)))))
