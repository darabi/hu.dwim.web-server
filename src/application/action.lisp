;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (constant :test 'string=) +action-id-parameter-name+  "_a")

(def class* action ()
  ()
  (:metaclass funcallable-standard-class))

(def (generic e) decorate-uri (uri thing)
  (:method progn (uri thing)
    ;; nop
    )
  (:method progn (uri (application application))
    (unless (scheme-of uri)
      (setf (scheme-of uri) (default-uri-scheme-of application))))
  (:method progn (uri (frame frame))
    (setf (uri-query-parameter-value uri +frame-id-parameter-name+) (id-of frame))
    (setf (uri-query-parameter-value uri +frame-index-parameter-name+) (frame-index-of frame)))
  (:method progn (uri (action action))
    (setf (uri-query-parameter-value uri +action-id-parameter-name+) (id-of action)))
  (:method-combination progn))

(def (function i) decorate-uri-with-frame-id (uri)
  (add-query-parameter-to-uri uri +frame-id-parameter-name+ (id-of *frame*))
  uri)

(def function %action-uri (action-lambda scheme)
  (bind ((uri (make-uri))
         (action (make-instance 'action)))
    (set-funcallable-instance-function action action-lambda)
    (decorate-uri uri *application*)
    (decorate-uri uri *session*)
    (decorate-uri uri *frame*)
    (decorate-uri uri action)
    (awhen *component*
      (dolist (component (nreverse (collect-component-ancestors it :include-self #t)))
        (decorate-uri uri component)))
    (when scheme
      (setf (scheme-of uri) scheme))
    uri))

(def macro action-uri ((&key scheme) &body body)
  `(%action-uri (lambda () ,@body) ,scheme))

(def macro action-href ((&key scheme) &body body)
  `(print-uri-to-string (%action-uri (lambda () ,@body) ,scheme)))
