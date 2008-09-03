;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def method make-reference-label ((reference standard-object-inspector-reference) (class dmm::entity) (instance prc::persistent-object))
  (reuse-standard-object-inspector-reference reference)
  (localized-instance-name instance))

(def method make-reference-label ((reference standard-object-list-inspector-reference) (class dmm::entity) (instance prc::persistent-object))
  (localized-instance-name instance))
