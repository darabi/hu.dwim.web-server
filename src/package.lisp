;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui.system)

(defpackage :hu.dwim.wui.shortcut
  (:use :common-lisp
        :cl-def))

(defpackage :hu.dwim.wui
  (:use :common-lisp
        :closer-mop
        :anaphora
        :alexandria
        :metabang-bind
        :defclass-star
        :computed-class
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
        :contextl
        :hu.dwim.util
        :hu.dwim.wui.system)

  (:shadow #:class-prototype
           #:class-slots
           #:class-precedence-list
           #:|defun|
           )

  (:shadowing-import-from :trivial-garbage
                          #:make-hash-table)

  (:shadowing-import-from :cl-syntax-sugar
                          #:define-syntax))

(defpackage :hu.dwim.wui.user
  (:use :common-lisp
        :iterate
        :local-time
        :bordeaux-threads
        :trivial-garbage
        :hu.dwim.util
        :hu.dwim.wui
        :hu.dwim.wui.system
        :hu.dwim.wui.shortcut
        )

  (:shadowing-import-from :trivial-garbage
                          #:make-hash-table))

(use-package :hu.dwim.wui :hu.dwim.wui.shortcut)
