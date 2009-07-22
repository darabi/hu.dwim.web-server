;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (dmm:operation e) expand-instance-operation (dmm::standard-operation)
  ())

(def layered-method make-expand-reference-command ((reference reference-component) (class dmm::entity) target expansion)
  (if (dmm::authorize-operation 'expand-instance-operation :-entity- class)
      (call-next-method)
      (make-reference-label reference class target)))

(def method make-reference-label ((reference standard-object-inspector-reference) (class dmm::entity) (instance prc::persistent-object))
  (localized-instance-name (target-of reference)))

(def method make-reference-label ((reference standard-object-list-inspector-reference) (class dmm::entity) (instance prc::persistent-object))
  (localized-instance-name instance))
