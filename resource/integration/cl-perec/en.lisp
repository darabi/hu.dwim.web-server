;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Persistent process

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
      (dmm::paused      "Process is paused")))

  (icon-label.start-process "Start")
  (icon-tooltip.start-process "Start the process")

  (icon-label.continue-process "Continue")
  (icon-tooltip.continue-process "Continue the process")

  (icon-label.cancel-process "Cancel")
  (icon-tooltip.cancel-process "Cancel the process")

  (icon-label.pause-process "Pause")
  (icon-tooltip.pause-process "Pause the process"))

;;;;;;
;;; Dimension

(def resources en
  (dimension-name.time "idő")
  (dimension-name.validity "hatályosság")

  (class-name.dimension "dimenzió")
  (class-name.pivot-table-dimension-axis-component "dimenzió alapú pivot tábla tengely")

  (slot-name.dimension "dimenzió"))

;;;;;;
;;; Filter

(def resources en
  (label.select-clause "select")
  (label.from-clause "from")
  (label.where-clause "where"))
