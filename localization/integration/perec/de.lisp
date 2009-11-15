;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Persistent process

(def localization de
  (process.message.waiting-for-other-subject "Process is waiting for other subject.")
  (process.message.waiting "Process is currently waiting.")
  (process.message.report-process-state (process)
    (ecase (hu.dwim.meta-model::element-name-of (hu.dwim.meta-model::process-state-of process))
      ;; a process in 'running state may not reach this point
      (hu.dwim.meta-model::finished    "Process finished normally")
      (hu.dwim.meta-model::failed      "Process failed")
      (hu.dwim.meta-model::broken      "Process was stopped due to technical failures")
      (hu.dwim.meta-model::cancelled   "Process has been cancelled")
      (hu.dwim.meta-model::in-progress "Process is in progress")
      (hu.dwim.meta-model::paused      "Process is paused")))

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

(def localization de
  (dimension-name.time "idő")
  (dimension-name.validity "hatályosság")

  (class-name.dimension "dimenzió")
  (class-name.pivot-table-dimension-axis-component "dimenzió alapú pivot tábla tengely")

  (slot-name.dimension "dimenzió"))

;;;;;;
;;; Filter

(def localization de
  (label.select-clause "select")
  (label.from-clause "from")
  (label.where-clause "where"))
