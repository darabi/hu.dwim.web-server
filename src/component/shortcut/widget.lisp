;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui.shortcut)

;;;;;;
;;; A list of possibly conflicting shortcuts

(def macro-shortcuts
  (command/widget command)
  (command-bar/widget command-bar)
  (expandible/widget expandible)
  (image/widget image)
  (splitter/widget splitter)
  (wrapper/widget wrapper)
  (top/widget top))
