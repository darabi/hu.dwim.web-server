(in-package :cl-user)

(defpackage #:hu.dwim.wui-test
  (:nicknames :wui-test)
  (:use
   :common-lisp
   :stefil
   :alexandria
   :metabang-bind
   :cl-def
   :cl-yalog
   :hu.dwim.wui
   :hu.dwim.wui.system
   :iterate
   :closer-mop
   :iolib
   :babel
   :babel-streams
   :cl-syntax-sugar
   :cl-quasi-quote
   :cl-quasi-quote-xml
   )
  (:shadowing-import-from :cl-syntax-sugar
   #:define-syntax
   )
  (:shadow
   #:parent
   #:test
   #:uri
   #:deftest
   )
  (:export
   #:test
   ))

(in-package :wui-test)

(rename-package :hu.dwim.wui :hu.dwim.wui '(:wui))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; import all the internal symbol of WUI
  (iter (for symbol :in-package #.(find-package :hu.dwim.wui) :external-only nil)
        (when (and (eq (symbol-package symbol) #.(find-package :hu.dwim.wui))
                   (not (find-symbol (symbol-name symbol) #.(find-package :wui-test))))
          (import symbol))))
