;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def class* application (broker-with-url-prefix)
  ((entry-points))
  (:metaclass closer-mop:funcallable-standard-class))

(def constructor application
  (closer-mop:set-funcallable-instance-function
    self
    (lambda (request)
      (application-handler self request))))

(def function application-handler (application request)
  (bind (((:values matches? relative-path) (matches-url-prefix? application request)))
    (when matches?
      )))
