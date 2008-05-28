;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Process

(def special-variable *process-component*)

(def component process-component (content-component)
  ((form)
   (closure/cc nil)
   (answer-continuation nil)))

(def render process-component ()
  (with-slots (form closure/cc answer-continuation content) self
    (unless content
      (setf answer-continuation
            (bind ((*process-component* self))
              (unless closure/cc
                (setf closure/cc (cl-delico::make-closure/cc (cl-walker:walk-form `(lambda () ,form)))))
              (cl-delico:with-call/cc
                (funcall closure/cc)))))
    (unless (and content
                 answer-continuation)
      (setf content (make-instance 'string-component :value "Process finished")))
    (call-next-method)))

(def (macro e) make-process-component (&rest args &key form &allow-other-keys)
  (remove-from-plistf args :form)
  `(make-instance 'process-component :form ',form ,@args))

(def (macro e) call (component)
  `(let/cc k
     (setf (content-of *process-component*) ,component)
     k))

(def (function e) answer (component value)
  (bind ((*process-component* (find-ancestor-component-with-type component 'process-component)))
    (setf (answer-continuation-of *process-component*)
          (cl-delico:kall (answer-continuation-of *process-component*) value))))

(def component answer-command-component (command-component)
  ((icon (make-icon-component 'answer :label "Answer"))
   (action)
   (value nil)))

(def constructor answer-command-component ()
  (with-slots (icon action value) self
    (setf action (make-action (answer self value)))))

(def method cl-quasi-quote::collect-slots-for-syntax-node-emitting-form ((node answer-command-component))
  (remove 'action (call-next-method) :key #'slot-definition-name))
