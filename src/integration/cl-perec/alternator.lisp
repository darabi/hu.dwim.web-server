;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(dmm::defoperation switch-to-alternative-view-operation (dmm::standard-operation)
  ()
  "Base operation for all model related operations.")

(def method make-replace-with-alternative-command :around ((component alternator-component) alternative)
  (when (dmm::authorize-operation 'switch-to-alternative-view-operation)
    (call-next-method)))
