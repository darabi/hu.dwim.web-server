;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; manager

(def (function e) make-manager (type)
  (tab-container/widget ()
    (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Search"))
      (make-filter type))
    (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Create"))
      (make-maker type))))
