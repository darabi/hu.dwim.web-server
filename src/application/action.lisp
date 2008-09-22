;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

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

(def method call-action :around (application session frame (action action))
  (bind ((*action* action))
    (call-next-method)))

(def class* component-related-action (action)
  ((component))
  (:metaclass funcallable-standard-class))

(def method call-action :around (application session frame (action component-related-action))
  (with-restored-component-environment (component-of action)
    (call-next-method)))

(def (macro e) make-action (&body body)
  (with-unique-names (action)
    `(bind ((,action (make-instance 'action)))
       (set-funcallable-instance-function ,action (lambda () ,@body))
       ,action)))

(def (macro e) make-component-related-action (component &body body)
  (with-unique-names (action)
    `(bind ((,action (make-instance 'component-related-action :component ,component)))
       (set-funcallable-instance-function ,action (lambda () ,@body))
       ,action)))

(def (macro e) make-action-uri ((&key scheme delayed-content) &body body)
  `(action-to-uri (make-action ,@body) :scheme ,scheme :delayed-content ,delayed-content))

(def (macro e) make-action-href ((&key scheme delayed-content) &body body)
  `(print-uri-to-string
    (make-action-uri (:scheme ,scheme :delayed-content ,delayed-content)
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

(def (function e) action-to-uri (action &key scheme delayed-content)
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
    uri))

(def (function e) action-to-href (action &key scheme delayed-content)
  (print-uri-to-string (action-to-uri action :scheme scheme :delayed-content delayed-content)))

(def (macro e) js-to-lisp-rpc (&environment env &body body)
  (bind ((walked-body (cl-walker:walk-form `(progn ,@body) nil (cl-walker:make-walk-environment env)))
         (free-variable-references (cl-walker:collect-variable-references walked-body :type 'cl-walker:free-variable-reference-form))
         (variable-names (remove-duplicates (mapcar [cl-walker:name-of !1]
                                                    free-variable-references)))
         (query-parameters (mapcar [unique-js-name (string-downcase !1)]
                                   variable-names)))
    `(progn
       `js-inline*(wui.io.xhr-post
                   (create
                    :content (create ,@(list ,@(iter (for variable-name :in variable-names)
                                                     (for query-parameter :in query-parameters)
                                                     (collect `(quote ,query-parameter))
                                                     (collect ;;`str ,(concatenate 'string (lisp-name-to-js-name variable-name) ".toString()")
                                                              ;; FIXME qq is broken, needs the ` reader. see qq/test/js.lisp for the detailed TODO
                                                              `js-inline*(.toString ,variable-name)
                                                              ))))
                    :url ,(make-action-href (:delayed-content #t)
                            (with-request-params ,(mapcar [list !1 !2]
                                                          variable-names
                                                          query-parameters)
                              ,@body))
                    :load (lambda (response args)
                            ;; TODO process the return value, possible ajax replacements, etc
                            )))
       nil)))
