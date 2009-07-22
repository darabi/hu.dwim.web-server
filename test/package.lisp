(in-package :cl-user)

(defpackage #:hu.dwim.wui.test
  (:nicknames :wui.test)

  (:use :alexandria
        :babel
        :babel-streams
        :cl-def
        :cl-l10n
        :cl-quasi-quote
        :cl-quasi-quote-js
        :cl-quasi-quote-xml
        :cl-syntax-sugar
        :cl-yalog
        :closer-mop
        :common-lisp
        :hu.dwim.util
        :hu.dwim.wui
        :hu.dwim.wui.shortcut
        :hu.dwim.wui.system
        :iolib
        :iterate
        :metabang-bind
        :stefil)

  (:export #:test)

  (:shadowing-import-from :cl-syntax-sugar
                          #:define-syntax)

  (:shadow #:parent
           #:test
           #:test
           #:uri))

(in-package :wui.test)

(rename-package :hu.dwim.wui :hu.dwim.wui '(:wui))

(def function setup-readtable ()
  (hu.dwim.wui::setup-readtable)
  (enable-string-quote-syntax #\｢ #\｣))

(register-readtable-for-swank
 '("HU.DWIM.WUI.TEST") 'setup-readtable)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; import all the internal symbol of WUI
  (iter (for symbol :in-package #.(find-package :hu.dwim.wui) :external-only nil)
        (when (and (eq (symbol-package symbol) #.(find-package :hu.dwim.wui))
                   (not (find-symbol (symbol-name symbol) #.(find-package :wui.test))))
          (import symbol))))
