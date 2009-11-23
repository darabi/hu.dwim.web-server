;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui+cl-graph+cl-typesetting
  :class hu.dwim.system
  :depends-on (:cl-graph
               :hu.dwim.wui
               :cl-typesetting)
  :components ((:module "source"
                :components ((:module "component"
                              :components ((:module "widget"
                                            :components ((:file "graph")))))))))
