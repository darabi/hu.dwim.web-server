;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (function o) find-action-from-request (frame)
  (bind ((action-id (parameter-value +action-id-parameter-name+)))
    (when action-id
      (app.debug "Found action-id parameter ~S, looking it up in frame ~A" action-id frame)
      (bind ((action (gethash action-id (action-id->action-of frame))))
        (if action
            (progn
              (app.debug "Looked up as valid action ~A" action)
              (values action action-id))
            (values))))))

(def class* action (string-id-for-funcallable-mixin)
  ()
  (:metaclass funcallable-standard-class))

(def method call-action :before (application session frame (action action))
  (assert (boundp '*action*))
  (setf *action* action))

(def (macro e) make-action (&body body)
  (with-unique-names (action)
    `(bind ((,action (make-instance 'action)))
       (set-funcallable-instance-function ,action (named-lambda action-body ()
                                                    ,@body))
       ,action)))

;; don't call these make-foo because that would be misleading. it's not only about making, but also
;; about registering the actions, and the time the registering happens is quite important. leave
;; the make-foo nameing convention for sideffect-free stuff...
(def (macro e) action/uri ((&key scheme path application-relative-path delayed-content) &body body)
  `(register-action/uri (make-action ,@body) :scheme ,scheme :path ,path :application-relative-path ,application-relative-path
                        :delayed-content ,delayed-content))

(def (macro e) action/href ((&key scheme delayed-content) &body body)
  `(print-uri-to-string
    (action/uri (:scheme ,scheme :delayed-content ,delayed-content)
      ,@body)))

(def function register-action (frame action)
  (assert frame)
  (assert-session-lock-held (session-of frame))
  (bind ((action-id->action (action-id->action-of frame))
         (action-id (insert-with-new-random-hash-table-key action-id->action action +action-id-length+)))
    (setf (id-of action) action-id)
    (app.dribble "Registered action with id ~S in frame ~A" action-id frame))
  action)

(def (function e) register-action/uri (action &key scheme path application-relative-path delayed-content)
  (bind ((uri (clone-request-uri)))
    (register-action *frame* action)
    (decorate-uri uri *application*)
    (decorate-uri uri *session*)
    (decorate-uri uri *frame*)
    (decorate-uri uri action)
    (when scheme
      (setf (scheme-of uri) scheme))
    (when application-relative-path
      (when path
        (error "REGISTER-ACTION/URI was called woth both PATH, and APPLICATION-RELATIVE-PATH arguments at the same time"))
      (setf (path-of uri) (path-prefix-of *application*))
      (append-path-to-uri uri application-relative-path))
    (when path
      (setf (path-of uri) path))
    (setf (uri-query-parameter-value uri +delayed-content-parameter-name+)
          (if delayed-content "t" nil))
    uri))

(def (function e) register-action/href (action &key scheme path application-relative-path delayed-content)
  (print-uri-to-string (register-action/uri action :scheme scheme :path path :application-relative-path application-relative-path
                                            :delayed-content delayed-content)))

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

(def function render-action-js-event-handler (event-name id action &key action-arguments js target-dom-node
                                                         (ajax (typep action 'action))
                                                         (send-client-state #t))
  (check-type ajax boolean)
  (check-type target-dom-node (or null string))
  (flet ((make-constant-form (value)
           (check-type value string)
           (make-instance 'hu.dwim.walker:constant-form :value value))
         (%default-onclick-js (ajax target-dom-node send-client-state?)
           (lambda (href)
             ;; KLUDGE: this condition prevents firing obsolete actions, they are not necessarily
             ;;         removed by destroy when simply replaced by some other content, this may leak memory on the cleint side
             `js(progn     ; TODO reinstate this: when (dojo.byId ,id)
                  (wui.io.action ,href
                                 :event event
                                 :ajax ,(to-boolean ajax)
                                 :target-dom-node ,target-dom-node
                                 :send-client-state ,(to-boolean send-client-state?)))
             ;; TODO add a *special* that collects the args of all action's and runs a js side loop to process the literal arrays
             ;; TODO add special handling of apply to qq so that the 'this' arg of .apply is not needed below (wui.io.action twice)
             ;; TODO do something like this below instead of the above, once qq properly emits commas in the output of (create ,@emtpy-list)
             #+nil
             (if (and (eq ajax #t)
                      send-client-state?)
                 `js(wui.io.action ,href :event event)
                 `js(.apply wui.io.action ,href
                            (create
                             :event event
                             ,@(append (unless (eq ajax #t)
                                         (list (make-instance 'hu.dwim.walker:constant-form :value :ajax)
                                               (make-instance 'hu.dwim.quasi-quote.js:js-unquote :form 'ajax)))
                                       (unless send-client-state?
                                         (list (make-instance 'hu.dwim.walker:constant-form :value :send-client-state)
                                               (make-instance 'hu.dwim.walker:constant-form :value '|false|))))))))))
    (bind ((href (etypecase action
                   (null
                    (unless js
                      (error "~S was called with NIL action and no custom js" 'render-action-js-event-handler))
                    nil)
                   (action
                    (apply 'register-action/href action action-arguments))
                   (uri
                    ;; TODO: wastes resources. store back the printed uri? see below also...
                    (print-uri-to-string action)))))
      `js(on-load
          (wui.connect ,(etypecase id
                          (cons   (make-array-form (mapcar #'make-constant-form id)))
                          (string id))
                       ,event-name
                       (lambda (event)
                         ;; TODO fix qq js and inline %default-onclick-js here
                         ,(apply (or js (%default-onclick-js (and (ajax-enabled? *application*)
                                                                  ajax)
                                                             target-dom-node send-client-state))
                                 (when href (list href)))))))))

;; TODO this is broken
(def (macro e) js-to-lisp-rpc (&environment env &body body)
  (bind ((walked-body (hu.dwim.walker:walk-form `(progn ,@body) :environment (hu.dwim.walker:make-walk-environment env)))
         (free-variable-references (hu.dwim.walker:collect-variable-references walked-body :type 'hu.dwim.walker:free-variable-reference-form))
         (variable-names (remove-duplicates (mapcar [hu.dwim.walker:name-of !1]
                                                    free-variable-references)))
         (query-parameters (mapcar [unique-js-name (string-downcase !1)]
                                   variable-names)))
    ` `js-inline(wui.io.xhr-post
                  (create
                   :content (create ,@(list ,@(iter (for variable-name :in variable-names)
                                                    (for query-parameter :in query-parameters)
                                                    (collect `(quote ,query-parameter))
                                                    (collect `js-inline(.toString ,variable-name)))))
                   :url ,(action/href (:delayed-content #t)
                           (with-request-parameters ,(mapcar [list !1 !2]
                                                             variable-names
                                                             query-parameters)
                             ,@body))
                   :load (lambda (response args)
                           ;; TODO process the return value, possible ajax replacements, etc
                           )))))
