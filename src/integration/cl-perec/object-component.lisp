;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def method reuse-standard-object-instance ((class prc::persistent-class) (instance prc::persistent-object))
  (if (prc::persistent-p instance)
      (prc::load-instance instance)
      (call-next-method)))

(def method hash-key-for ((instance prc::persistent-object))
  (prc::oid-of instance))
