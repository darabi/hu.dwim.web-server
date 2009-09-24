;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.component
  :class hu.dwim.system
  :setup-readtable-function-name "hu.dwim.wui::setup-readtable"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain (sans advertising clause)"
  :description "Extension to the HTTP application server to become an HTTP component based user interface server for the world wide web."
  :long-description "Provides various components, layouts, widgets, charts, books, model documentation components, meta components. Components have server and client side state and behavior."
  :depends-on (:contextl
               :hu.dwim.wui.application)
  :components ((:module "source"
                :components ((:module "util"
                              :components ((:file "book")
                                           (:file "csv")
                                           (:file "dictionary")
                                           (:file "project")
                                           #+sbcl(:file "object-size")
                                           (:file "place")))
                             (:module "component"
                              :depends-on ("util")
                              :components ((:module "api"
                                            :components ((:file "api")
                                                         (:file "component" :depends-on ("api" "mop"))
                                                         (:file "computed" :depends-on ("component"))
                                                         (:file "debug" :depends-on ("component"))
                                                         (:file "interaction" :depends-on ("component"))
                                                         (:file "mop")
                                                         (:file "number" :depends-on ("api"))
                                                         (:file "response" :depends-on ("component"))
                                                         #+sbcl(:file "sbcl-ctor-kludge" :depends-on ("mop"))
                                                         (:file "string" :depends-on ("api"))))
                                           (:module "mixin"
                                            :depends-on ("api")
                                            :components ((:file "border")
                                                         (:file "cell" :depends-on ("column"))
                                                         (:file "cloneable")
                                                         (:file "closable")
                                                         (:file "collapsible")
                                                         (:file "column")
                                                         (:file "command")
                                                         (:file "command-bar")
                                                         (:file "content")
                                                         (:file "context-menu")
                                                         (:file "disableable")
                                                         (:file "draggable")
                                                         (:file "editable")
                                                         (:file "exportable")
                                                         (:file "footer")
                                                         (:file "header")
                                                         (:file "hideable")
                                                         (:file "id" :depends-on ("refreshable"))
                                                         (:file "initargs")
                                                         (:file "layer")
                                                         (:file "menu")
                                                         (:file "menu-bar")
                                                         (:file "mouse")
                                                         (:file "node" :depends-on ("tree"))
                                                         (:file "page-navigation-bar")
                                                         (:file "parent")
                                                         (:file "refreshable")
                                                         (:file "remote" :depends-on ("id"))
                                                         (:file "renderable")
                                                         (:file "resizable")
                                                         (:file "row")
                                                         (:file "scrollable")
                                                         (:file "selectable")
                                                         (:file "style" :depends-on ("remote"))
                                                         (:file "table")
                                                         (:file "title" :depends-on ("refreshable"))
                                                         (:file "tooltip")
                                                         (:file "top")
                                                         (:file "tree")
                                                         (:file "value")))
                                           (:module "layout"
                                            :depends-on ("mixin")
                                            :components ((:file "alternator" :depends-on ("layout"))
                                                         (:file "cell" :depends-on ("layout"))
                                                         (:file "container" :depends-on ("layout"))
                                                         (:file "empty" :depends-on ("layout"))
                                                         (:file "flow" :depends-on ("layout"))
                                                         (:file "layout")
                                                         (:file "list" :depends-on ("layout"))
                                                         (:file "node" :depends-on ("layout"))
                                                         (:file "nodrow" :depends-on ("layout"))
                                                         (:file "row" :depends-on ("cell"))
                                                         (:file "table" :depends-on ("row"))
                                                         (:file "tree" :depends-on ("node"))
                                                         (:file "treeble" :depends-on ("row"))
                                                         (:file "xy" :depends-on ("layout"))))
                                           (:module "widget"
                                            :depends-on ("layout")
                                            :components ((:file "alternator" :depends-on ("reference" "command" "menu"))
                                                         (:file "border")
                                                         (:file "button")
                                                         (:file "cell" :depends-on ("table" "row" "column"))
                                                         (:file "collapsible" :depends-on ("command"))
                                                         (:file "column")
                                                         (:file "command" :depends-on ("icon"))
                                                         (:file "command-bar" :depends-on ("command"))
                                                         (:file "content")
                                                         (:file "debug" :depends-on ("menu" "frame" "inline" "replace-target"))
                                                         (:file "element" :depends-on ("command"))
                                                         (:file "external-link")
                                                         (:file "field")
                                                         (:file "frame" :depends-on ("top"))
                                                         (:file "help" :depends-on ("icon"))
                                                         (:file "icon")
                                                         (:file "image")
                                                         (:file "inline")
                                                         (:file "internal-error" :depends-on ("message" "command-bar" "command"))
                                                         (:file "list")
                                                         (:file "menu" :depends-on ("command"))
                                                         (:file "message")
                                                         (:file "name-value")
                                                         (:file "node")
                                                         (:file "nodrow" :depends-on ("treeble" "column" "cell"))
                                                         (:file "page-navigation-bar" :depends-on ("command"))
                                                         (:file "panel" :depends-on ("message"))
                                                         (:file "path")
                                                         (:file "reference")
                                                         (:file "replace-target" :depends-on ("command"))
                                                         (:file "row" :depends-on ("table"))
                                                         (:file "scroll")
                                                         (:file "scroll-bar")
                                                         (:file "splitter")
                                                         (:file "tab-container" :depends-on ("command-bar"))
                                                         (:file "table")
                                                         (:file "title")
                                                         (:file "tool-bar")
                                                         (:file "tooltip")
                                                         (:file "top" :depends-on ("message"))
                                                         (:file "tree")
                                                         (:file "tree-level")
                                                         (:file "treeble")
                                                         (:file "widget")
                                                         #+nil
                                                         ((:file "authentication")
                                                          (:file "expression")
                                                          (:file "extended-table")
                                                          (:file "file-up-and-download")
                                                          (:file "frame-size-breakdown")
                                                          (:file "pivot-table")
                                                          (:file "timestamp-range")
                                                          (:file "wizard"))))
                                           (:module "chart"
                                            :depends-on ("widget")
                                            :components ((:file "chart")
                                                         (:file "column" :depends-on ("chart"))
                                                         (:file "flow" :depends-on ("chart"))
                                                         (:file "line" :depends-on ("chart"))
                                                         (:file "pie" :depends-on ("chart"))
                                                         (:file "radar" :depends-on ("chart"))
                                                         (:file "scatter" :depends-on ("chart"))
                                                         (:file "stock" :depends-on ("chart"))
                                                         (:file "structure" :depends-on ("chart"))))
                                           (:module "presentation"
                                            :depends-on ("api")
                                            :components ((:file "editor")
                                                         (:file "filter")
                                                         (:file "finder")
                                                         (:file "inspector")
                                                         (:file "invoker")
                                                         (:file "maker")
                                                         (:file "selector")
                                                         (:file "viewer")
                                                         (:file "xxx")))
                                           (:module "text"
                                            :depends-on ("widget" "presentation")
                                            :components ((:file "text")
                                                         (:file "book" :depends-on ("text"))
                                                         (:file "chapter" :depends-on ("text"))
                                                         (:file "glossary" :depends-on ("text"))
                                                         (:file "index" :depends-on ("text"))
                                                         (:file "paragraph" :depends-on ("text"))
                                                         (:file "toc" :depends-on ("text"))))
                                           (:module "source"
                                            :depends-on ("widget" "presentation")
                                            :components ((:file "class")
                                                         (:file "demo")
                                                         (:file "dictionary")
                                                         (:file "file")
                                                         (:file "function")
                                                         (:file "generic")
                                                         (:file "method")
                                                         (:file "model")
                                                         (:file "module")
                                                         (:file "name")
                                                         (:file "package")
                                                         (:file "pathname")
                                                         (:file "project")
                                                         (:file "repl")
                                                         (:file "slot")
                                                         (:file "shell-script")
                                                         (:file "system")
                                                         (:file "type")
                                                         (:file "uri")
                                                         (:file "variable")))
                                           (:module "primitive"
                                            :depends-on ("widget" "presentation")
                                            :components ((:file "abstract" :depends-on ("primitive"))
                                                         (:file "editor" :depends-on ("primitive"))
                                                         (:file "filter" :depends-on ("primitive"))
                                                         (:file "inspector" :depends-on ("primitive"))
                                                         (:file "maker" :depends-on ("primitive"))
                                                         (:file "primitive")
                                                         (:file "viewer" :depends-on ("primitive"))))
                                           (:module "place"
                                            :depends-on ("widget" "presentation")
                                            :components ((:file "place")
                                                         (:file "editor" :depends-on ("place"))
                                                         (:file "filter" :depends-on ("place"))
                                                         (:file "inspector" :depends-on ("place"))
                                                         (:file "maker" :depends-on ("place"))
                                                         (:file "viewer" :depends-on ("place"))))
                                           (:module "object"
                                            :depends-on ("primitive" "place")
                                            :components ((:file "xxx")
                                                         #+nil
                                                         ((:file "object")
                                                          (:file "filter" :depends-on ("object"))
                                                          (:file "inspector" :depends-on ("object"))
                                                          (:file "list-inspector" :depends-on ("object"))
                                                          (:file "maker" :depends-on ("object"))
                                                          (:file "manager" :depends-on ("object"))
                                                          (:file "pivot-table" :depends-on ("object"))
                                                          (:file "process" :depends-on ("object"))
                                                          (:file "reference" :depends-on ("object"))
                                                          (:file "tree-filter" :depends-on ("object"))
                                                          (:file "tree-inspector" :depends-on ("object")))))
                                           (:module "shortcut"
                                            :depends-on ("object")
                                            :components ((:file "layout")
                                                         (:file "widget")))
                                           ;; KLUDGE: kill this
                                           (:file "xxx" :depends-on ("shortcut"))))))))
