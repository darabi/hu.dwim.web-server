;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.test
  :class hu.dwim.test-system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain (sans advertising clause)"
  :depends-on (:hu.dwim.wui
               :hu.dwim.wui.component.test))

;; TODO: revive
#+nil
(defmethod perform :after ((op test-op) (system (eql (find-system :hu.dwim.wui.http.test))))
  (let ((*package* (find-package :hu.dwim.wui)))
    (eval (read-from-string "(progn
                               ;; set dojo to the latest available
                               (setf *dojo-directory-name* (find-latest-dojo-directory-name (asdf:system-relative-pathname :hu.dwim.wui \"www/\")))
                               (setf (log-level 'wui) +debug+)
                               (setf *debug-on-error* t)
                               ;; KLUDGE ASDF wraps everything in a WITH-COMPILATION-UNIT which eventually prevents starting the
                               ;; tests on SBCL due to The Big Compiler Lock, so we spawn here a thread to run the tests.
                               (bordeaux-threads:make-thread
                                 (lambda ()
                                   (hu.dwim.stefil:funcall-test-with-feedback-message 'wui.test:test))))")))
  (warn "Set WUI log level to +debug+; enabled server-side debugging"))