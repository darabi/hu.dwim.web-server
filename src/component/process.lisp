;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Process

;; TODO: try to kill this variable, if possible?!
(def (special-variable e) *standard-process-component*)

(def component standard-process-component (content-mixin
                                           user-messages-mixin
                                           commands-mixin)
  ((form)
   (closure/cc nil)
   (answer-continuation nil)))

(def (macro e) standard-process (&body forms)
  `(make-instance 'standard-process-component :form '(progn ,@forms)))

(def refresh standard-process-component
  (bind (((:slots form closure/cc) -self-))
    (setf closure/cc (cl-delico::make-closure/cc (cl-walker:walk-form `(lambda () ,form))))))

(def render-xhtml standard-process-component
  (bind (((:read-only-slots closure/cc answer-continuation command-bar content) -self-))
    (unless content
      (setf answer-continuation
            (bind ((*standard-process-component* -self-))
              (with-call/cc
                (funcall closure/cc)))))
    (unless (and content answer-continuation)
      (setf content "Process finished"))
    <div ,(render-user-messages -self-)
         ,(render content)
         ,(render command-bar)>))

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

(def (function/cc e) call-component (component &key answer-commands)
  (setf answer-commands (ensure-list answer-commands))
  (let/cc k
    (setf (content-of *standard-process-component*) component)
    (replace-answer-commands *standard-process-component* answer-commands)
    k))

(def (generic e) answer-component (component value)
  (:method ((component component) value)
    (answer-component (find-ancestor-component-with-type component 'standard-process-component) value))

  (:method ((component standard-process-component) value)
    (bind ((*standard-process-component* component))
      (setf (answer-continuation-of *standard-process-component*)
            (kall (answer-continuation-of *standard-process-component*) (force value))))))

;;;;;;
;;; Answer command

(def component answer-command-component (command-component)
  ((icon (icon answer :label "Answer")) ;; TODO localize
   (action nil)
   (value nil)))

(def constructor answer-command-component ()
  (bind (((:slots icon action value) -self-))
    (unless action
      (setf action (make-action (answer-component -self- value))))))

(def (macro e) answer-command (content &body forms)
  `(make-instance 'answer-command-component :content ,content :value (delay ,@forms)))
