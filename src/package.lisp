;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui.system)

(defpackage :hu.dwim.wui.shortcut
  (:use :cl-def
        :common-lisp))

(defpackage :hu.dwim.wui
  (:use :alexandria
        :anaphora
        :babel
        :babel-streams
        :bordeaux-threads
        :cl-def
        :cl-delico
        :cl-l10n
        :cl-l10n.lang
        :cl-quasi-quote
        :cl-quasi-quote-js
        :cl-quasi-quote-xml
        :cl-syntax-sugar
        :cl-yalog
        :closer-mop
        :common-lisp
        :computed-class
        :contextl
        :defclass-star
        :hu.dwim.util
        :hu.dwim.wui.system
        :iterate
        :metabang-bind
        :trivial-garbage)

  (:shadow #:class-prototype
           #:class-slots
           #:class-precedence-list
           #:|defun|)

  (:shadowing-import-from :trivial-garbage
                          #:make-hash-table)

  (:shadowing-import-from :cl-syntax-sugar
                          #:define-syntax))

(defpackage :hu.dwim.wui.user
  (:use :bordeaux-threads
        :common-lisp
        :hu.dwim.util
        :hu.dwim.wui
        :hu.dwim.wui.shortcut
        :hu.dwim.wui.system
        :iterate
        :local-time
        :trivial-garbage)

  (:shadowing-import-from :trivial-garbage
                          #:make-hash-table))

(use-package :hu.dwim.wui :hu.dwim.wui.shortcut)
