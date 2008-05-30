;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Process

(def special-variable *standard-process-component*)

(def component standard-process-component (content-component)
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
    (call-next-method)))

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
            (kall (answer-continuation-of *standard-process-component*) value)))))

(def component answer-command-component (command-component)
  ((icon (make-icon-component 'answer :label "Answer"))
   (action)
   (value nil)))

(def constructor answer-command-component ()
  (with-slots (icon action value) -self-
    (setf action (make-action (answer -self- value)))))

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
                               (dmm::start-persistent-process-in-context)))))))
    (unless (and content answer-continuation)
      (setf content (make-instance 'label-component :component-value "Process finished")))
    (call-next-method)))

(def macro dwim-meta-model:show-to-subject (component &optional subject)
  "Shows a user interface component to the given subject."
  (once-only (subject)
    `(dmm::show-and-wait ,component
                         (or (not ,subject)
                             (and (dmm::has-authenticated-session)
                                  (eq ,subject (dmm::current-effective-subject))))
                         (make-instance 'dmm::wait-for-subject
                                        :subject (or ,subject
                                                     (dmm::current-effective-subject))))))

(def macro dwim-meta-model:show-to-subject-expression (component &optional expression)
  "Shows a user interface component to any one of the subjects matching to the given expression"
  `(dmm::show-and-wait ,component
                       (and (dmm::has-authenticated-session)
                            (bind ((dmm::-authenticated-subject- (dmm::current-authenticated-subject))
                                   (dmm::-effective-subject- (dmm::current-effective-subject)))
                              ,expression))
                       (or (dmm::select-wait-for-expression :wait-for-subject #t :expression ',expression)
                           (dmm::make-wait-for-expression :wait-for-subject #t :expression ',expression))))

(def macro dwim-meta-model:show-and-wait (component &optional (condition t) (wait-reason nil))
  `(progn
     (dmm::persistent-process-wait dmm::*process* ,wait-reason)
     (if ,condition
         (let (self)
           (setf self ,component)
           (call self))
         (let/cc k k))
     (assert (eq (dmm::element-name-of (dmm::process-state-of dmm::*process*)) 'dmm::running))))

(def method answer ((component persistent-process-component) value)
  (bind ((*standard-process-component* component))
    (rdbms::with-transaction
      (prc::revive-instance (process-of component))
      (dmm::with-new-persistent-process-context (:renderer component :process (process-of component))
        (setf (answer-continuation-of *standard-process-component*)
              (nth-value 1 (dmm::continue-persistent-process-in-context)))))))
