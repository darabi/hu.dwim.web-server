;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;; Process stuff
(def resources en
  (process.message.waiting-for-other-subject "Process is waiting for other subject.")
  (process.message.waiting "Process is currently waiting.")
  (process.message.report-process-state (process)
    (ecase (dmm::element-name-of (dmm::process-state-of process))
      ;; a process in 'running state may not reach this point
      (dmm::finished    "Process finished normally")
      (dmm::failed      "Process failed")
      (dmm::broken      "Process was stopped due to technical failures")
      (dmm::cancelled   "Process has been cancelled")
      (dmm::in-progress "Process is in progress")
      (dmm::paused      "Process is paused"))))
