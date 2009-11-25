;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui+hu.dwim.reader
  :class hu.dwim.system
  :depends-on (:hu.dwim.reader+hu.dwim.syntax-sugar
               :hu.dwim.wui)
  :components ((:module "source"
                :components ((:module "component"
                              :components ((:module "source"
                                            :components ((:file "form")
                                                         (:file "repl" :depends-on ("form"))))))))))
