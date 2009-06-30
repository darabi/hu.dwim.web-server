(in-package :cl-user)

(defpackage #:hu.dwim.wui.test
  (:nicknames :wui.test)

  (:use :common-lisp
        :stefil
        :alexandria
        :metabang-bind
        :cl-def
        :cl-yalog
        :cl-l10n
        :iterate
        :closer-mop
        :iolib
        :babel
        :babel-streams
        :cl-syntax-sugar
        :cl-quasi-quote
        :cl-quasi-quote-js
        :cl-quasi-quote-xml
        :hu.dwim.wui
        :hu.dwim.wui.system
        :hu.dwim.wui.shortcut
        )

  (:export #:test)

  (:shadowing-import-from :cl-syntax-sugar
                          #:define-syntax
                          )

  (:shadow #:parent
           #:test
           #:uri
           #:test
           ))

(in-package :wui.test)

(rename-package :hu.dwim.wui :hu.dwim.wui '(:wui))

(def function setup-readtable ()
  (hu.dwim.wui::setup-readtable))

(register-readtable-for-swank
 '("HU.DWIM.WUI.TEST") 'setup-readtable)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; import all the internal symbol of WUI
  (iter (for symbol :in-package #.(find-package :hu.dwim.wui) :external-only nil)
        (when (and (eq (symbol-package symbol) #.(find-package :hu.dwim.wui))
                   (not (find-symbol (symbol-name symbol) #.(find-package :wui.test))))
          (import symbol))))
