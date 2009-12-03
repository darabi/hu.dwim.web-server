;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.http
  :class hu.dwim.system
  :description "An iolib based HTTP server."
  :long-description "Provides error handling, response compression, static file serving, quasi quoted JavaScript and quasi quoted XML serving."
  :depends-on (:babel
               :babel-streams
               :bordeaux-threads
               :cffi
               :cl-fad
               :hu.dwim.common
               :hu.dwim.computed-class+hu.dwim.defclass-star
               :hu.dwim.def.namespace
               :hu.dwim.def+cl-l10n
               :hu.dwim.def+contextl
               :hu.dwim.def+hu.dwim.delico
               :hu.dwim.logger
               :hu.dwim.quasi-quote.xml+hu.dwim.quasi-quote.js
               :hu.dwim.syntax-sugar
               :hu.dwim.util.all
               :iolib
               :iolib.pathnames
               :iolib.sockets
               :iolib.os
               :local-time
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
                                           (:file "timestring-parsing")
                                           (:file "zlib")))
                             (:module "http"
                              :depends-on ("logger" "util")
                              :components ((:file "accept-headers" :depends-on ("variables"))
                                           (:file "conditions" :depends-on ("variables"))
                                           (:file "error-handling" :depends-on ("variables" "util"))
                                           (:file "production")
                                           (:file "request-parsing" :depends-on ("request-response" "accept-headers" "uri"))
                                           (:file "request-response" :depends-on ("variables" "util"))
                                           (:file "uri" :depends-on ("variables"))
                                           (:file "util" :depends-on ("variables"))
                                           (:file "variables")))
                             (:module "server"
                              :depends-on ("http")
                              :components ((:file "api" :depends-on ("conditions"))
                                           (:file "brokers" :depends-on ("server"))
                                           (:file "cgi" :depends-on ("brokers"))
                                           (:file "conditions")
                                           (:file "file-serving" :depends-on ("variables"))
                                           (:file "js-serving" :depends-on ("js-util" "file-serving"))
                                           (:file "js-util" :depends-on ("variables"))
                                           (:file "misc" :depends-on ("variables"))
                                           (:file "server" :depends-on ("variables"))
                                           (:file "variables" :depends-on ("api"))))))))
