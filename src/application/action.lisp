;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (constant :test 'string=) +action-id-parameter-name+ "_a")
(def constant +action-id-length+ 8)

(def (constant :test 'string=) +delayed-content-parameter-name+ "_d")

(def function request-for-delayed-content? (&optional (request *request*))
  "A delayed content request is supposed to render stuff to the same frame that was delayed at the main request (i.e. tooltips)."
  (bind ((value (request-parameter-value request +delayed-content-parameter-name+)))
    (and value
         (not (string= value "")))))

(def (function o) find-action-from-request (frame)
  (bind ((action-id (parameter-value +action-id-parameter-name+)))
    (when action-id
      (app.debug "Found action-id parameter ~S" action-id)
      (bind ((action (gethash action-id (action-id->action-of frame))))
        (if action
            (progn
              (app.debug "Looked up as valid action ~A" action)
              action)
            (values))))))

(def class* action (string-id-for-funcallable-mixin)
  ()
  (:metaclass funcallable-standard-class))

(def function make-action-using-lambda (action-lambda)
  (bind ((action (make-instance 'action)))
    (set-funcallable-instance-function action action-lambda)
    action))

(def (macro e) make-action (&body body)
  `(make-action-using-lambda (lambda () ,@body)))

(def (macro e) make-action-uri ((&key scheme delayed-content (ajax-aware *default-ajax-aware-client*)) &body body)
  `(action-to-uri (make-action ,@body) :scheme ,scheme :delayed-content ,delayed-content :ajax-aware ,ajax-aware))

(def (macro e) make-action-href ((&key scheme delayed-content (ajax-aware *default-ajax-aware-client*)) &body body)
  `(print-uri-to-string
    (make-action-uri (:scheme ,scheme :delayed-content ,delayed-content :ajax-aware ,ajax-aware)
      ,@body)))

(def function register-action (frame action)
  (assert (or (not (boundp '*frame*))
              (null *frame*)
              (eq *frame* frame)))
  (assert-session-lock-held (session-of frame))
  (bind ((action-id->action (action-id->action-of frame))
         (action-id (insert-with-new-random-hash-table-key action-id->action action +action-id-length+)))
    (setf (id-of action) action-id)
    (app.dribble "Registered action with id ~S in frame ~A" action-id frame))
  action)

(def (generic e) decorate-uri (uri thing)
  (:method progn (uri thing)
    ;; nop
    )
  (:method progn (uri (application application))
    (unless (scheme-of uri)
      (setf (scheme-of uri) (default-uri-scheme-of application)))
    (setf (path-of uri) (path-prefix-of application)))
  (:method progn (uri (frame frame))
    (setf (uri-query-parameter-value uri +frame-id-parameter-name+) (id-of frame))
    (setf (uri-query-parameter-value uri +frame-index-parameter-name+) (frame-index-of frame)))
  (:method progn (uri (action action))
    (setf (uri-query-parameter-value uri +action-id-parameter-name+) (id-of action))
    (setf (uri-query-parameter-value uri +frame-index-parameter-name+) (next-frame-index-of *frame*)))
  (:method-combination progn))

(def (function e) action-to-uri (action &key scheme delayed-content (ajax-aware *default-ajax-aware-client*))
  (bind ((uri (clone-uri (uri-of *request*))))
    (register-action *frame* action)
    (decorate-uri uri *application*)
    (decorate-uri uri *session*)
    (decorate-uri uri *frame*)
    (decorate-uri uri action)
    (when scheme
      (setf (scheme-of uri) scheme))
    (setf (uri-query-parameter-value uri +delayed-content-parameter-name+)
          (if delayed-content "t" nil))
    (setf (uri-query-parameter-value uri +ajax-aware-client-parameter-name+)
          (if ajax-aware "t" nil))
    uri))

(def (function e) action-to-href (action &key scheme delayed-content (ajax-aware *default-ajax-aware-client*))
  (print-uri-to-string (action-to-uri action :scheme scheme :delayed-content delayed-content :ajax-aware ajax-aware)))
