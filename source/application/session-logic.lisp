;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (with-macro* eo) with-session-logic (&key ensure-session (requires-valid-session #t) (lock-session #t))
  (assert (and (boundp '*application*)
               *application*
               (boundp '*session*)
               (boundp '*frame*))
          () "May not use WITH-SESSION-LOGIC outside the dynamic extent of an application")
  (bind ((application *application*)
         (session nil)
         (session-instance nil)
         (session-cookie-exists? #f)
         (invalidity-reason nil)
         (new-session? #f))
    (app.debug "WITH-SESSION-LOGIC speaking, request is delayed-content? ~A, ajax-aware? ~A" *delayed-content-request* *ajax-aware-request*)
    (with-lock-held-on-application (application)
      (setf (values session session-cookie-exists? invalidity-reason session-instance)
            (find-session-from-request application))
      (when (and (not session)
                 (eq invalidity-reason :nonexistent)
                 ensure-session)
        (setf session (make-new-session application))
        (register-session application session)
        (setf new-session? #t))
      ;; FIXME locking the session below should happen while having the lock on the application here
      )
    (abort-request-unless-still-valid)
    (setf *session* session)
    (if session
        (bind ((local-time:*default-timezone* (client-timezone-of session)))
          (incf (requests-to-sessions-count-of application))
          (restart-case
              (bind ((response (if lock-session
                                   (progn
                                     (app.debug "WITH-SESSION-LOGIC is locking session ~A as requested" session)
                                     ;; TODO check if locking would hang, throw error if so
                                     (with-lock-held-on-session (session)
                                       (when (is-request-still-valid?)
                                         (call-in-application-environment application session #'-body-))))
                                   (progn
                                     (app.debug "WITH-SESSION-LOGIC is NOT locking session ~A, it wasn't requested" session)
                                     (call-in-application-environment application session #'-body-)))))
                (when (and new-session?
                           response)
                  (decorate-application-response application response))
                response)
            (delete-current-session ()
              :report (lambda (stream)
                        (format stream "Delete session ~A and rety handling the request" session))
              (mark-expired session)
              (invoke-retry-handling-request-restart))))
        (if (or requires-valid-session
                (not (eq invalidity-reason :nonexistent)))
            (bind ((response (handle-request-to-invalid-session application session invalidity-reason)))
              (decorate-application-response application response)
              response)
            (call-in-application-environment application nil #'-body-)))))

(def (with-macro* eo) with-frame-logic (&key (requires-valid-frame #t) (ensure-frame #f))
  (assert (and *application* *session* (boundp '*frame*)) () "May not use WITH-FRAME-LOGIC without a proper session in the environment")
  (app.debug "WITH-FRAME-LOGIC speaking, requires-valid-frame ~A, ensure-frame ~A" requires-valid-frame ensure-frame)
  (bind ((application *application*)
         (session *session*)
         ((:values frame frame-id-parameter-received? invalidity-reason frame-instance) (when session
                                                                                          (find-frame-for-request session))))
    (setf *frame* frame)
    (app.debug "WITH-FRAME-LOGIC looked up frame ~A from session ~A" frame session)
    (if frame
        (-body-)
        (cond
          ((and requires-valid-frame
                (or (not session)
                    frame-id-parameter-received?))
           (handle-request-to-invalid-frame application session frame-instance invalidity-reason))
          ((and ensure-frame
                *session*)
           ;; set up a new frame and fall through to the entry points to set up to their favour
           (setf frame (make-new-frame application session))
           (setf (id-of frame) (insert-with-new-random-hash-table-key (frame-id->frame-of session)
                                                                      frame +frame-id-length+))
           (register-frame application session frame)
           (setf *frame* frame)
           (-body-))
          (t
           (-body-))))))

(def (with-macro* eo) with-action-logic ()
  (assert (and *application* *session* *frame*) () "May not use WITH-ACTION-LOGIC without a proper application/session/frame dynamic environment")
  (bind ((application *application*)
         (session *session*)
         (frame *frame*))
    (assert-session-lock-held session)
    ;; TODO here? find its place...
    (notify-activity session)
    (labels ((convert-to-primitive-response* (response)
               (app.debug "Calling CONVERT-TO-PRIMITIVE-RESPONSE for ~A while still inside the WITH-LOCK-HELD-ON-SESSION's and WITH-ACTION-LOGIC's dynamic scope" response)
               (decorate-application-response *application* response)
               (convert-to-primitive-response response))
             (call-body ()
               (values (convert-to-primitive-response* (-body-)))))
      (declare (inline call-body))
      (if frame
          (progn
            (restart-case
                (progn
                  (setf *frame* frame)
                  (notify-activity frame)
                  (process-client-state-sinks frame (query-parameters-of *request*))
                  (bind ((action (find-action-from-request frame))
                         (incoming-frame-index (parameter-value +frame-index-parameter-name+))
                         (current-frame-index (frame-index-of frame))
                         (next-frame-index (next-frame-index-of frame)))
                    (unless (stringp current-frame-index)
                      (setf current-frame-index (integer-to-string current-frame-index)))
                    (unless (stringp next-frame-index)
                      (setf next-frame-index (integer-to-string next-frame-index)))
                    (app.debug "Incoming frame-index is ~S, current is ~S, next is ~S, action is ~A" incoming-frame-index current-frame-index next-frame-index action)
                    (cond
                      ((and action
                            incoming-frame-index)
                       (bind ((original-frame-index nil))
                         (unwind-protect-case ()
                             (if (equal incoming-frame-index next-frame-index)
                                 (progn
                                   (app.dribble "Found an action and frame is in sync...")
                                   (unless *delayed-content-request*
                                     (setf original-frame-index (step-to-next-frame-index frame)))
                                   (app.dribble "Calling action...")
                                   (bind ((response (call-action application session frame action)))
                                     (when (typep response 'response)
                                       (return-from with-action-logic
                                         (convert-to-primitive-response* response)))))
                                 (return-from with-action-logic
                                   (convert-to-primitive-response* (handle-request-to-invalid-frame application session frame :out-of-sync))))
                           (:abort
                            ;; TODO the problem at hand is this: when the app specific error handler is called the stack is not yet unwinded
                            ;; so this REVERT-STEP-TO-NEXT-FRAME-INDEX is not yet called, therefore the page it renders will point to an invalid
                            ;; frame index after this unwind block is executed.
                            ;; but on the other hand without this uwp, the "retry rendering this request" restart is broken...
                            ;; we chose the lesser badness here and don't do the revert, so break the restart instead of the user visible error page
                            #+nil
                            (when original-frame-index
                              (revert-step-to-next-frame-index frame original-frame-index))))))
                      (incoming-frame-index
                       (unless (equal incoming-frame-index current-frame-index)
                         (return-from with-action-logic
                           (convert-to-primitive-response*
                            (handle-request-to-invalid-frame application session frame :out-of-sync)))))
                      ;; at the time the frame is first registered, there's no frame index param in the url, so just fall through here and
                      ;; end up at the entry points.
                      )
                    (app.dribble "Action logic fell through, proceeding to the thunk...")))
              (delete-current-frame ()
                :report (lambda (stream)
                          (format stream "Delete frame ~A" frame))
                (mark-expired frame)
                (invoke-retry-handling-request-restart)))
            (call-body))
          (if *ajax-aware-request*
              ;; we can't find a valid frame but received an ajax aware request. for now treat this as an
              ;; error until a valid use-case requires something else...
              (frame-not-found-error)
              (call-body))))))

;;;;;;
;;; invalid request handling

(def method handle-request-to-invalid-session ((application application) session invalidity-reason)
  (app.debug "Default HANDLE-REQUEST-TO-INVALID-SESSION is sending a redirect response to ~A" application)
  (make-redirect-response-for-current-application))

(def method handle-request-to-invalid-frame ((application application) session frame invalidity-reason)
  (app.dribble "Default HANDLE-REQUEST-TO-INVALID-FRAME speeking, invalidity-reason is ~S" invalidity-reason)
  (if (eq invalidity-reason :out-of-sync)
      (bind ((refresh-href   (print-uri-to-string (make-uri-for-current-frame)))
             (new-frame-href (print-uri-to-string (make-uri-for-new-frame)))
             (args (list refresh-href new-frame-href)))
        (app.debug "Default HANDLE-REQUEST-TO-INVALID-FRAME is sending a frame out of sync response")
        (emit-simple-html-document-http-response (:status +http-not-acceptable+
                                                          :headers '#.+disallow-response-caching-header-values+)
          (apply-localization-function 'render-frame-out-of-sync-error args))
        (make-do-nothing-response))
      (progn
        (app.debug "Default HANDLE-REQUEST-TO-INVALID-FRAME is sending a redirect response to ~A" application)
        (make-redirect-response-for-current-application))))

(def method handle-request-to-invalid-action ((application application) session frame action invalidity-reason)
  (app.debug "Default HANDLE-REQUEST-TO-INVALID-ACTION is sending a redirect response to ~A" application)
  (make-redirect-response-for-current-application))

(def (function e) invoke-delete-current-frame-restart ()
  (invoke-restart (find-restart 'delete-current-frame)))

(def (function e) invoke-delete-current-session-restart ()
  (invoke-restart (find-restart 'delete-current-session)))

(def method register-frame ((application application) (session session) (frame frame))
  (assert-session-lock-held session)
  (assert (null (session-of frame)) () "The frame ~A is already registered to a session" frame)
  (assert (or (not (boundp '*frame*))
              (null *frame*)
              (eq *frame* frame)))
  (bind ((frame-id->frame (frame-id->frame-of session)))
    ;; TODO purge frames
    (bind ((frame-id (insert-with-new-random-hash-table-key frame-id->frame frame +frame-id-length+)))
      (setf (id-of frame) frame-id)
      (setf (session-of frame) session)
      (app.dribble "Registered frame with id ~S" (id-of frame))
      frame)))

(def method register-session ((application application) (session session))
  (assert-application-lock-held application)
  (assert (null (application-of session)) () "The session ~A is already registered to an application" session)
  (assert (or (not (boundp '*session*))
              (null *session*)
              (eq *session* session)))
  (bind ((session-id->session (session-id->session-of application)))
    (when (> (hash-table-count session-id->session)
             (maximum-sessions-count-of application))
      (too-many-sessions application))
    (bind ((session-id (insert-with-new-random-hash-table-key session-id->session session +session-id-length+)))
      (setf (id-of session) session-id)
      (setf (application-of session) application)
      (app.dribble "Registered session with id ~S" (id-of session))
      session)))

(def method delete-session ((application application) (session session))
  (assert-application-lock-held application)
  (assert (eq (application-of session) application))
  (app.dribble "Deleting session ~A" session)
  (bind ((session-id->session (session-id->session-of application)))
    (remhash (id-of session) session-id->session))
  (values))

(def method purge-sessions :around (application)
  (with-thread-name " / PURGE-SESSIONS"
    (call-next-method)))

(def (method o) purge-sessions ((application application))
  (app.dribble "Purging the sessions of ~S" application)
  ;; this method must be called while not holding any session or application lock
  (assert (not (is-lock-held? (lock-of application))) () "You must NOT have a lock on the application when calling PURGE-SESSIONS (or on any of its sessions)!")
  (setf (sessions-last-purged-at-of application) (get-monotonic-time))
  (let ((deleted-sessions (list))
        (live-sessions (list)))
    (with-lock-held-on-application (application)
      (iter (for (session-id session) :in-hashtable (session-id->session-of application))
            (if (is-session-alive? session)
                (push session live-sessions)
                (block deleting-session
                  (with-layered-error-handlers ((lambda (error &key &allow-other-keys)
                                                  (app.error "Could not delete expired/invalid session ~A of application ~A, got error ~A" session application error))
                                                (lambda (&key &allow-other-keys)
                                                  (return-from deleting-session)))
                    (delete-session application session)
                    (push session deleted-sessions))))))
    (dolist (session deleted-sessions)
      (block noifying-session
        (with-layered-error-handlers ((lambda (error &key &allow-other-keys)
                                        (app.error "Error happened while notifying session ~A of application ~A about its exiration, got error ~A" session application error))
                                      (lambda (&key &allow-other-keys)
                                        (return-from noifying-session)))
          (with-lock-held-on-session (session)
            (notify-session-expiration application session)))))
    (dolist (session live-sessions)
      (block purging-session-frames
        (with-layered-error-handlers ((lambda (error &key &allow-other-keys)
                                        (app.error "Error happened while purging frames of ~A of application ~A. Got error ~A" session application error))
                                      (lambda (&key &allow-other-keys)
                                        (return-from purging-session-frames)))
          (with-lock-held-on-session (session)
            (purge-frames application session)))))
    (values)))

(def (function e) mark-all-sessions-expired (application)
  (with-lock-held-on-application (application)
    (iter (for (key session) :in-hashtable (session-id->session-of application))
          (mark-expired session))))
