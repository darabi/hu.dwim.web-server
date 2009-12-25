;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (generic e) make-new-session (application))
(def (generic e) make-new-frame (application session))

(def (generic e) register-session (application session))
(def (generic e) register-frame (application session frame))

(def (generic e) purge-sessions (application))
(def (generic e) purge-frames (application session))

(def (generic e) delete-session (application session))

(def (generic e) session-class (application)
  (:documentation "Returns a list of the session mixin classes.

Custom implementations should look something like this:
\(def method session-class list ((app your-application))
  'your-session-mixin)")
  (:method-combination list))

(def (generic e) call-in-application-environment (application session thunk)
  (:documentation "Everything related to an application goes through this method, so it can be used to set up wrappers like WITH-TRANSACTION. The SESSION argument may or may not be a valid session.")
  (:method (application session thunk)
    (app.dribble "CALL-IN-APPLICATION-ENVIRONMENT is calling the thunk")
    (funcall thunk))
  (:method :around (application session thunk)
    (when (boundp 'call-in-application-environment/guard)
      (cerror "ignore" "~S has been called recursively" 'call-in-application-environment))
    (bind ((call-in-application-environment/guard #t))
      (declare (special call-in-application-environment/guard))
      (call-next-method))))

(def (generic e) call-in-post-action-environment (application session frame thunk)
  (:documentation "This call wraps entry points and rendering, but does not wrap actions. The SESSION argument may or may not be a valid session.")
  (:method (application session frame thunk)
    (app.dribble "CALL-IN-POST-ACTION-ENVIRONMENT is calling the thunk")
    (funcall thunk)))

(def (generic e) call-in-entry-point-environment (application session thunk)
  (:method (application session thunk)
    (bind ((*inside-user-code* #t))
      (funcall thunk))))

(def (generic e) call-action (application session frame action)
  (:method (application session frame (action function))
    (funcall action))
  (:method :around (application session frame action)
    (bind ((*inside-user-code* #t))
      (call-next-method))))

(def type session-invalidity-reason ()
  `(member :nonexistent :timed-out :invalidated))

(def type frame-invalidity-reason ()
  `(member :nonexistent :timed-out :invalidated :out-of-sync))

(def type action-invalidity-reason ()
  `(member :nonexistent :timed-out :invalidated))

(def (generic e) handle-request-to-invalid-session (application session invalidity-reason)
  (:method :before (application session invalidity-reason)
    (check-type invalidity-reason session-invalidity-reason)
    (check-type session (or null session))))

(def (generic e) handle-request-to-invalid-frame (application session frame invalidity-reason)
  (:method :before (application session frame invalidity-reason)
    (check-type invalidity-reason frame-invalidity-reason)
    (check-type frame (or null frame))))

(def (generic e) handle-request-to-invalid-action (application session frame action invalidity-reason)
  (:method :before (application session frame action invalidity-reason)
    (check-type invalidity-reason action-invalidity-reason)
    (check-type action (or null action))))

(def generic entry-point-equals-for-redefinition (a b)
  (:method (a b)
    #f)
  (:method :around (a b)
           (if (eql (class-of a) (class-of b))
               (call-next-method)
               #f)))

;;;;;;
;;; authentication (needs an APPLICATION-WITH-LOGIN-SUPPORT)

(def (generic e) login (application session login-data)
  (:method (application session login-data)
    ;; nop by default
    ))

(def (generic e) logout (application session)
  (:method (application session)
    ;; nop by default
    ))

(def (generic e) authenticate (application potentially-unregistered-web-session login-data)
  (:documentation "Should return something non-NIL in case the authentication was successful. WUI treats the return value as a mere generalized boolean, but it's also stored in the AUTHENTICATE/RETURN-VALUE slot of the web session for later use by user code."))

(def (generic e) is-logged-in? (session)
  (:documentation "Should return T if we are in an authenticated session opened by a succesful LOGIN.")
  (:method (session)
    #f))
