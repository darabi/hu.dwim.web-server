;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui+hu.dwim.serializer
  :class hu.dwim.system
  :depends-on (:hu.dwim.serializer
               :hu.dwim.wui)
  :components ((:module "integration"
                :components ((:file "serializer")))))
