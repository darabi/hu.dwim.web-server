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
  (test/human-readable/invariant :file "/file")
  (test/human-readable/invariant :class "/class")
  (test/human-readable/invariant :function "/function")
  (test/human-readable/invariant (find-class 'class) "/class/COMMON-LISP:CLASS")
  (test/human-readable/invariant (find-slot 'server 'handler) "/class/HU.DWIM.WUI:SERVER/effective-slot/HU.DWIM.WUI::HANDLER")
  (test/human-readable/invariant (fdefinition 'list) "/function/COMMON-LISP:LIST")
  (test/human-readable/invariant (def project test-project :path (system-pathname :hu.dwim.wui)) "/project/HU.DWIM.WUI.TEST::TEST-PROJECT")
  (test/human-readable/invariant (def book test-book ()) "/book/HU.DWIM.WUI.TEST::TEST-BOOK"))
