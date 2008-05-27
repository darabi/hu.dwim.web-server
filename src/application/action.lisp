;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (constant :test 'string=) +action-id-parameter-name+  "_a")
(def constant +action-id-length+ 8)

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

(def class* action ()
  ((id nil))
  (:metaclass funcallable-standard-class))

(def print-object (action :identity #t :type #f)
  (print-object-for-string-id-mixin self))

(def function register-action (frame action)
  (assert (or (not (boundp '*frame*))
              (null *frame*)
              (eq *frame* frame)))
  (assert-session-lock-held (session-of frame))
  (bind ((action-id->action (action-id->action-of frame)))
    (bind ((action-id (insert-with-new-random-hash-table-key action-id->action action +action-id-length+)))
      (setf (id-of action) action-id)
      (app.dribble "Registered action with id ~S in frame ~A" (id-of action) frame)
      action)))

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

(def function action-to-uri (action &key scheme)
  (bind ((uri (clone-uri (uri-of *request*))))
    (register-action *frame* action)
    (decorate-uri uri *application*)
    (decorate-uri uri *session*)
    (decorate-uri uri *frame*)
    (decorate-uri uri action)
    (when scheme
      (setf (scheme-of uri) scheme))
    uri))

(def function action-to-href (action &key scheme)
  (print-uri-to-string (action-to-uri action :scheme scheme)))

(def function make-action-with-lambda (action-lambda)
  (bind ((action (make-instance 'action)))
    (set-funcallable-instance-function action action-lambda)
    action))

(def macro make-action (&body body)
  `(make-action-with-lambda (lambda () ,@body)))
