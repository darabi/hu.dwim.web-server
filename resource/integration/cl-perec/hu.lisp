;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Persistent process

(def resources hu
  (process.message.waiting-for-other-subject "A folyamat jelenleg másra várakozik.")
  (process.message.waiting "A folyamat jelenleg várakozik.")
  (process.message.report-process-state (process)
    (ecase (dmm::element-name-of (dmm::process-state-of process))
      ;; a process in 'running state may not reach this point
      (dmm::finished    "Folyamat normálisan befejeződött")
      (dmm::failed      "Folyamat hibára futott")
      (dmm::broken      "Folyamat technikai hiba miatt megállítva")
      (dmm::cancelled   "Folyamat felhasználó által leállítva")
      (dmm::in-progress "Folyamat folyamatban")
      (dmm::paused      "Folyamat félbeszakítva")))

  (icon-label.start-process "Elindítás")
  (icon-tooltip.start-process "A folyamat elindítása")

  (icon-label.continue-process "Folytatás")
  (icon-tooltip.continue-process "A folyamat folytatása")

  (icon-label.cancel-process "Elvetés")
  (icon-tooltip.cancel-process "A folyamat elvetése")

  (icon-label.pause-process "Felfüggesztés")
  (icon-tooltip.pause-process "A folyamat felüggesztése"))

;;;;;;
;;; Dimension

(def resources hu
  (dimension-name.time "time")
  (dimension-name.validity "validity")

  (class-name.dimension "dimension")
  (class-name.pivot-table-dimension-axis-component "pivot table dimension axis")

  (slot-name.dimension "dimension"))

;;;;;;
;;; Filter

(def resources hu
  (label.select-clause "mit")
  (label.from-clause "honnan")
  (label.where-clause "mikor"))
