;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui+hu.dwim.perec
  :class hu.dwim.system
  :depends-on (:cl-l10n
               :hu.dwim.perec+iolib
               :hu.dwim.meta-model
               :hu.dwim.wui)
  :components ((:module "integration"
                :components ((:module "perec"
                              :components ((:file "kludge")
                                           (:file "l10n")
                                           (:file "object-filter")
                                           (:file "process")
                                           #+nil
                                           ((:file "menu")
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
                                            (:file "dimensional")
                                            (:file "query-expression"))))))))
