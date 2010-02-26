;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui.test)

;;;;;;
;;; Test

(def suite (test/human-readable :in test))

(def test test/human-readable/invariant (object string)
  (is (equal string (serialize/human-readable object)))
  (is (equal object (deserialize/human-readable string))))

(def test test/human-readable/object ()
  (test/human-readable/invariant nil "")
  (test/human-readable/invariant :file "/FILE")
  (test/human-readable/invariant :class "/CLASS")
  (test/human-readable/invariant :function "/FUNCTION")
  (test/human-readable/invariant (find-class 'class) "/CLASS/COMMON-LISP:CLASS")
  (test/human-readable/invariant (find-slot 'server 'handler) "/CLASS/HU.DWIM.WUI:SERVER/EFFECTIVE-SLOT/HU.DWIM.WUI::HANDLER")
  (test/human-readable/invariant (fdefinition 'list) "/FUNCTION/COMMON-LISP:LIST")
  (test/human-readable/invariant (def project test-project :path (system-pathname :hu.dwim.wui)) "/PROJECT/HU.DWIM.WUI.TEST::TEST-PROJECT")
  (test/human-readable/invariant (def book test-book ()) "/BOOK/HU.DWIM.WUI.TEST::TEST-BOOK"))
