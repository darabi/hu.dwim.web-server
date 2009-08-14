;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Persistent process

(def localization hu
  (process.message.waiting-for-other-subject "A folyamat jelenleg másra várakozik.")
  (process.message.waiting "A folyamat jelenleg várakozik.")
  (process.message.report-process-state (process)
    (ecase (hu.dwim.meta-model::element-name-of (hu.dwim.meta-model::process-state-of process))
      ;; a process in 'running state may not reach this point
      (hu.dwim.meta-model::finished    "Folyamat normálisan befejeződött")
      (hu.dwim.meta-model::failed      "Folyamat hibára futott")
      (hu.dwim.meta-model::broken      "Folyamat technikai hiba miatt megállítva")
      (hu.dwim.meta-model::cancelled   "Folyamat felhasználó által leállítva")
      (hu.dwim.meta-model::in-progress "Folyamat folyamatban")
      (hu.dwim.meta-model::paused      "Folyamat félbeszakítva")))

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

(def localization hu
  (dimension-name.time "time")
  (dimension-name.validity "validity")

  (class-name.dimension "dimension")
  (class-name.pivot-table-dimension-axis-component "pivot table dimension axis")

  (slot-name.dimension "dimension"))

;;;;;;
;;; Filter

(def localization hu
  (label.select-clause "mit")
  (label.from-clause "honnan")
  (label.where-clause "mikor"))
