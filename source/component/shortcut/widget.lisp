;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui.shortcut)

;;;;;;
;;; A list of possibly conflicting shortcuts

(def macro-shortcuts
  (command/widget command)
  (command-bar/widget command-bar)
  (collapsible/widget collapsible)
  (image/widget image)
  (splitter/widget splitter)
  (inline-render-component/widget inline-render-component)
  (wrap-render-component/widget wrap-render-component)
  (inline-render-xhtml/widget inline-render-xhtml)
  (wrap-render-xhtml/widget wrap-render-xhtml)
  (top/widget top)
  (tab-container/widget tab-container)
  (tab-page/widget tab-page)
  (inline-xhtml-string-content/widget inline-xhtml-string-content)
  (quote-xml-string-content/widget quote-xml-string-content))
