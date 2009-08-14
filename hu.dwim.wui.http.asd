;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.wui.http
  :class hu.dwim.system
  :setup-readtable-function-name "hu.dwim.wui::setup-readtable"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain (sans advertising clause)"
  :description "Basic HTTP server to build user interfaces for the world wide web."
  :long-description "Provides error handling, compression, static file serving, quasi quoted JavaScript and quasi quoted XML serving."
  :depends-on (:babel
               :babel-streams
               :bordeaux-threads
               :cffi
               :cl-fad
               :hu.dwim.common-lisp
               :hu.dwim.computed-class
               :hu.dwim.def+cl-l10n
               :hu.dwim.def+contextl
               :hu.dwim.def+hu.dwim.logger
               :hu.dwim.defclass-star
               :hu.dwim.delico
               :hu.dwim.quasi-quote.xml+hu.dwim.quasi-quote.js
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :iolib
               :local-time
               :net-telent-date ; TODO: remove this dependency
               :parse-number
               :rfc2109
               :rfc2388-binary)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "configuration" :depends-on ("package"))
                             (:file "logger" :depends-on ("package"))
                             (:module "util"
                              :depends-on ("logger" "configuration" "duplicates")
                              :components ((:file "l10n" :depends-on ("util"))
                                           (:file "timer")
                                           (:file "util")
                                           (:file "zlib")))
                             (:module "http"
                              :depends-on ("logger" "util")
                              :components ((:file "accept-headers" :depends-on ("variables"))
                                           (:file "brokers" :depends-on ("server"))
                                           (:file "conditions" :depends-on ("variables"))
                                           (:file "error-handling" :depends-on ("variables" "util"))
                                           (:file "request-parsing" :depends-on ("request-response" "uri"))
                                           (:file "request-response" :depends-on ("variables" "util"))
                                           (:file "server" :depends-on ("request-parsing"))
                                           (:file "uri" :depends-on ("variables"))
                                           (:file "util" :depends-on ("variables"))
                                           (:file "variables")))
                             (:module "server"
                              :depends-on ("http")
                              :components ((:file "file-serving")
                                           (:file "js-serving" :depends-on ("js-util" "file-serving"))
                                           (:file "js-util")))))))
