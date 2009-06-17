;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Computed universe

(def computed-universe compute-as-in-session)

(def function ensure-session-computed-universe ()
  (or (computed-universe-of *session*)
      (setf (computed-universe-of *session*) (make-computed-universe))))

(def (macro e) compute-as (&body forms)
  `(compute-as* ()
     ,@forms))
(setf (get 'compute-as 'cc::computed-as-macro-p) #t)
(setf (get 'compute-as 'cc::primitive-compute-as-macro) 'compute-as-in-session*)

(def (macro e) compute-as* ((&rest args &key &allow-other-keys) &body forms)
  `(compute-as-in-session* (:universe (ensure-session-computed-universe) ,@args)
     (call-compute-as -self- (lambda () ,@forms))))
(setf (get 'compute-as* 'cc::computed-as-macro-p) #t)
(setf (get 'compute-as* 'cc::primitive-compute-as-macro) 'compute-as-in-session*)

(def (generic e) call-compute-as (component thunk)
  (:method ((self component) thunk)
    (funcall thunk)))
