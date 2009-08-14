;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui.shortcut)

;;;;;;
;;; A list of possibly conflicting shortcuts

(def macro-shortcuts
  (empty/layout empty)
  (horizontal-list/layout horizontal-list)
  (vertical-list/layout vertical-list)
  (container/layout container))
