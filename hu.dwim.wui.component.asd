;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.component
  :class hu.dwim.system
  :description "Provides a component based server side GUI framework which is rendered into HTML and JavaScript through HTTP."
  :long-description "Provides various components, layouts, widgets, charts, books, model documentation components, meta components. Components have server and client side state and behavior."
  :depends-on (:contextl
               :hu.dwim.wui.application
               :hu.dwim.util.authorization
               :hu.dwim.util.source
               :hu.dwim.util.standard-process)
  :components ((:module "source"
                :components ((:module "util"
                              :components ((:file "book")
                                           (:file "csv")
                                           (:file "definition")
                                           (:file "dictionary")
                                           #+sbcl(:file "object-size")
                                           (:file "human-readable" :depends-on ("book" "project"))
                                           (:file "place")
                                           (:file "project")
                                           (:file "selection")))
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
                                                         (:file "deep-arguments")
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
                                            :components ((:file "about" :depends-on ("widget"))
                                                         (:file "alternator" :depends-on ("reference" "command" "menu"))
                                                         (:file "authentication" :depends-on ("command"))
                                                         (:file "border")
                                                         (:file "button")
                                                         (:file "cell" :depends-on ("table" "row" "column"))
                                                         (:file "collapsible" :depends-on ("command" "content"))
                                                         (:file "column")
                                                         (:file "command" :depends-on ("icon"))
                                                         (:file "command-bar" :depends-on ("command"))
                                                         (:file "content")
                                                         (:file "debug" :depends-on ("menu" "frame" "inline" "replace-target"))
                                                         (:file "element" :depends-on ("command"))
                                                         (:file "external-link")
                                                         (:file "field")
                                                         (:file "file-up-and-download" :depends-on ("icon"))
                                                         (:file "frame" :depends-on ("top"))
                                                         (:file "frame-size-breakdown")
                                                         (:file "google")
                                                         (:file "help" :depends-on ("title" "icon"))
                                                         (:file "icon" :depends-on ("widget"))
                                                         (:file "image")
                                                         (:file "inline")
                                                         (:file "internal-error" :depends-on ("inline" "title" "message" "command-bar" "command"))
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
                                                         (:file "wizard" :depends-on ("icon"))
                                                         #+nil
                                                         ((:file "extended-table")
                                                          (:file "pivot-table")
                                                          (:file "timestamp-range")
                                                          )))
                                           (:module "presentation"
                                            :depends-on ("mixin")
                                            :components ((:file "editor" :depends-on ("presentation"))
                                                         (:file "evaluator" :depends-on ("presentation"))
                                                         (:file "filter" :depends-on ("presentation"))
                                                         (:file "finder" :depends-on ("presentation"))
                                                         (:file "inspector" :depends-on ("presentation"))
                                                         (:file "invoker" :depends-on ("presentation"))
                                                         (:file "maker" :depends-on ("presentation"))
                                                         (:file "presentation")
                                                         (:file "selector" :depends-on ("presentation"))
                                                         (:file "viewer" :depends-on ("presentation"))))
                                           (:module "primitive"
                                            :depends-on ("widget" "presentation")
                                            :components ((:file "presentation" :depends-on ("primitive"))
                                                         (:file "editor" :depends-on ("presentation"))
                                                         (:file "filter" :depends-on ("presentation"))
                                                         (:file "inspector" :depends-on ("presentation"))
                                                         (:file "maker" :depends-on ("presentation"))
                                                         (:file "primitive")
                                                         (:file "viewer" :depends-on ("presentation"))))
                                           (:module "place"
                                            :depends-on ("widget" "presentation")
                                            :components ((:file "editor" :depends-on ("presentation"))
                                                         (:file "filter" :depends-on ("presentation"))
                                                         (:file "inspector" :depends-on ("presentation"))
                                                         (:file "maker" :depends-on ("presentation"))
                                                         (:file "presentation")
                                                         (:file "viewer" :depends-on ("presentation"))))
                                           (:module "object"
                                            :depends-on ("primitive" "place")
                                            :components ((:file "editor" :depends-on ("presentation"))
                                                         (:file "filter" :depends-on ("presentation"))
                                                         (:file "inspector" :depends-on ("presentation"))
                                                         (:file "maker" :depends-on ("presentation"))
                                                         (:file "manager" :depends-on ("presentation"))
                                                         (:file "presentation")
                                                         (:file "process" :depends-on ("presentation"))
                                                         (:file "viewer" :depends-on ("presentation"))))
                                           (:module "sequence"
                                            :depends-on ("object")
                                            :components ((:file "filter" :depends-on ("presentation"))
                                                         (:file "inspector" :depends-on ("presentation"))
                                                         (:file "presentation")))
                                           (:module "tree"
                                            :depends-on ("object")
                                            :components ((:file "inspector" :depends-on ("presentation"))
                                                         (:file "presentation")))
                                           (:module "text"
                                            :depends-on ("object" "tree")
                                            :components ((:file "text")
                                                         (:file "book" :depends-on ("text"))
                                                         (:file "chapter" :depends-on ("text"))
                                                         (:file "glossary" :depends-on ("text"))
                                                         (:file "hyperlink" :depends-on ("text"))
                                                         (:file "index" :depends-on ("text"))
                                                         (:file "paragraph" :depends-on ("text"))
                                                         (:file "toc" :depends-on ("text"))))
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
                                           (:module "source"
                                            :depends-on ("tree" "sequence")
                                            :components ((:file "class")
                                                         (:file "component")
                                                         (:file "component-class")
                                                         (:file "definition")
                                                         (:file "demo")
                                                         (:file "dictionary")
                                                         (:file "file")
                                                         (:file "form")
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
                                                         (:file "shell-script")
                                                         (:file "slot")
                                                         (:file "system")
                                                         (:file "type")
                                                         (:file "uri")
                                                         (:file "variable")))))
                             ;; KLUDGE: kill this
                             (:file "xxx" :pathname "component/xxx.lisp" :depends-on ("component"))))))
