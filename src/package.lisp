;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui.system)

(defpackage :hu.dwim.wui

  (:shadowing-import-from :trivial-garbage
                          #:make-hash-table)

  (:use :hu.dwim.wui.system
        :common-lisp
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
        )

  (:shadow #:class-prototype
           #:class-slots
           #:class-precedence-list
           #:|defun|
           )

  (:shadowing-import-from :cl-syntax-sugar
                          #:define-syntax)

  (:export #:path-prefix-of
           #:id-of
           ))

(defpackage :hu.dwim.wui-user

  (:shadowing-import-from :trivial-garbage
                          #:make-hash-table)

  (:use :common-lisp
        :hu.dwim.wui
        :iterate
        :local-time
        :bordeaux-threads
        :trivial-garbage
        ))
