;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui.system)

(defpackage :hu.dwim.wui

  (:shadowing-import-from :trivial-garbage
    #:make-hash-table)

  (:use
   :common-lisp
   :closer-mop
   :alexandria
   :cl-def
   :cl-yalog
   :cl-syntax-sugar
   :iolib
   :defclass-star
   :metabang-bind
   :hu.dwim.wui.system
   :cl-quasi-quote
   :cl-quasi-quote-xml
   :bordeaux-threads
   :trivial-garbage
   :iterate
   :babel
   :babel-streams
   )

  (:shadowing-import-from :cl-syntax-sugar
   #:define-syntax)

  (:export
   ))

(defpackage :hu.dwim.wui-user

  (:shadowing-import-from :trivial-garbage
    #:make-hash-table)

  (:use
   :common-lisp
   :hu.dwim.wui
   :iterate
   :local-time
   :bordeaux-threads
   :trivial-garbage
   ))

#+nil(defpackage :hu.dwim.wui.lang
  (:nicknames :ucw.lang)
  (:import-from :ucw
    #:+missing-resource-css-class+
    #:define-js-resources)
  (:export
    #:+missing-resource-css-class+
    #:define-js-resources
    #:timestamp
    #:timestamp<>
    #:file-last-modification-timestamp<>))

#+nil(defpackage :hu.dwim.wui.tags
  (:documentation "UCW convience tags.")
  (:use)
  (:nicknames #:<ucw)
  (:export
   #:component-body
   #:render-component
   #:a
   #:area
   #:form
   #:input
   #:button
   #:simple-select
   #:select
   #:option
   #:textarea

   #:integer-range-select
   #:month-day-select
   #:month-select

   #:text
   #:password
   #:submit
   #:simple-form
   #:simple-submit

   #:localized
   #:script))

#+nil(defpackage :hu.dwim.wui.dojo-tags
  (:documentation "Dojo convience tags.")
  (:use)
  (:nicknames #:<dojo)
  (:export
   #:widget))

