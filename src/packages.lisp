;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui.system)

(defpackage :hu.dwim.wui

  (:shadowing-import-from :trivial-garbage
    #:make-hash-table)

  (:use
   :hu.dwim.wui.system
   :common-lisp
   :closer-mop
   :alexandria
   :metabang-bind
   :defclass-star
   :iterate
   :cl-def
   :cl-yalog
   :cl-syntax-sugar
   :cl-l10n
   :cl-l10n.lang
   :cl-quasi-quote
   :cl-quasi-quote-xml
   :cl-quasi-quote-js
   :cl-delico
   :bordeaux-threads
   :trivial-garbage
   :babel
   :babel-streams
   :iolib
   )

  (:shadowing-import-from :cl-syntax-sugar
   #:define-syntax)

  (:export
   #:path-prefix-of
   #:id-of
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

