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
                              :components ((:file "d-value")
                                           (:file "dimension")
                                           (:file "filter")
                                           (:file "inspector")
                                           (:file "kludge")
                                           (:file "l10n")
                                           (:file "presentation")
                                           (:file "process")
                                           (:file "table")))))))
