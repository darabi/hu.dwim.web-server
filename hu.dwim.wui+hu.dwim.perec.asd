;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.wui+hu.dwim.perec
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :depends-on (:cl-l10n
               :hu.dwim.perec
               :hu.dwim.meta-model
               :hu.dwim.wui)
  :components ((:module "integration"
                :components ((:module "perec"
                              :components (#+nil
                                           ((:file "kludge")
                                            (:file "factory")
                                            (:file "l10n")
                                            (:file "menu")
                                            (:file "place")
                                            (:file "reference")
                                            (:file "editable")
                                            (:file "exportable")
                                            (:file "expression")
                                            (:file "alternator")
                                            (:file "object-component")
                                            (:file "object-inspector")
                                            (:file "object-list-inspector")
                                            (:file "object-tree-inspector")
                                            (:file "object-maker")
                                            (:file "object-filter")
                                            (:file "process")
                                            (:file "dimensional")
                                            (:file "query-expression"))))))))
