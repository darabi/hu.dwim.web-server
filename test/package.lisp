;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.wui.test
  (:use :babel
        :babel-streams
        :cl-l10n
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.logger
        :hu.dwim.quasi-quote
        :hu.dwim.quasi-quote.js
        :hu.dwim.quasi-quote.xml
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.wui
        :iolib)

  (:shadowing-import-from :hu.dwim.syntax-sugar
                          #:define-syntax)

  (:shadow #:parent
           #:test
           #:test
           #:uri))

(in-package :hu.dwim.wui.test)

(rename-package :hu.dwim.wui :hu.dwim.wui '(:wui))

(def function setup-readtable ()
  (hu.dwim.wui::setup-readtable)
  (enable-string-quote-syntax))

(register-readtable-for-swank
 '(:hu.dwim.wui.test) 'setup-readtable)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; import all the internal symbol of WUI
  (iter (for symbol :in-package #.(find-package :hu.dwim.wui) :external-only nil)
        (when (and (eq (symbol-package symbol) #.(find-package :hu.dwim.wui))
                   (not (find-symbol (symbol-name symbol) #.(find-package :hu.dwim.wui.test))))
          (import symbol))))
