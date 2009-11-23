;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui+stefil
  :class hu.dwim.system
  :depends-on (:hu.dwim.stefil
               :hu.dwim.wui)
  :components ((:module "source"
                :components ((:module "component"
                              :components ((:module "source"
                                            :components ((:file "test")))))))))
