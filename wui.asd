;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :cl-user)

;;;;;;
;;; System definitions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((try (system)
           (unless (asdf:find-system system nil)
             (warn "Trying to install required dependency: ~S" system)
             (when (find-package :asdf-install)
               (funcall (read-from-string "asdf-install:install") system))
             (unless (asdf:find-system system nil)
               (error "The ~A system requires ~A." (or *compile-file-pathname* *load-pathname*) system)))
           (asdf:operate 'asdf:load-op system)))
    (try :cl-syntax-sugar)
    (try :alexandria)))

(defpackage :hu.dwim.wui.system
  (:export #:*load-as-production-p*
           #:project-relative-pathname
           )

  (:use :common-lisp
        :asdf
        :cl-syntax-sugar
        :alexandria
        ))

(in-package :hu.dwim.wui.system)

(defun project-relative-pathname (path)
  (merge-pathnames path (component-pathname (find-system :wui))))

(defparameter *load-as-production-p* t
  "When T, load the WUI lisp files so that it will be used in a production system. This means that debug-only blocks are dropped and log levels and various variables are initialized accordingly.")

(defclass wui-source-file (cl-source-file-with-readtable)
  ())

(defmethod perform :around ((op operation) (component wui-source-file))
  (let ((*features* *features*))
    (unless *load-as-production-p*
      (pushnew :debug *features*))
    (handler-bind
        (#+sbcl (sb-ext:compiler-note #'muffle-warning))
      (call-next-method))))

(defclass wui-system (system-with-readtable)
  ((test-system :initform :wui.core.test :initarg :test-system :accessor test-system-of)))

(defmacro defsystem* (name &rest body &key
                      (default-component-class 'wui-source-file)
                      (class 'wui-system)
                      (setup-readtable-function "hu.dwim.wui::setup-readtable")
                      &allow-other-keys)
  (remove-from-plistf body :default-component-class :class)
  `(defsystem ,name
     :default-component-class ,default-component-class
     :class ,class
     :setup-readtable-function ,setup-readtable-function
     ,@body))

;;;;;;
;;; WUI server systems 

(defsystem* :wui-http-server
  :description "Basic HTTP server to build user interfaces for the world wide web."
  :long-description "Provides error handling, compression, static file serving, quasi quoted JavaScript and quasi quoted XML serving."
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Levente Mészáros <levente.meszaros@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD (sans advertising clause)"
  :test-system :wui-http-server-test
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "duplicates" :depends-on ("package"))
                 (:file "configuration" :depends-on ("package"))
                 (:file "logging" :depends-on ("package"))
                 (:module "util"
                  :components ((:file "zlib")
                               (:file "timer")
                               (:file "utils")
                               (:file "l10n" :depends-on ("utils")))
                  :depends-on ("configuration" "duplicates"))
                 (:module "http"
                  :components ((:file "variables")
                               (:file "uri")
                               (:file "accept-headers")
                               (:file "utils" :depends-on ("variables"))
                               (:file "conditions" :depends-on ("variables"))
                               (:file "error-handling" :depends-on ("variables"))
                               (:file "request-response" :depends-on ("variables"))
                               (:file "request-parsing" :depends-on ("request-response"))
                               (:file "server" :depends-on ("request-parsing"))
                               (:file "brokers" :depends-on ("server")))
                  :depends-on ("logging" "util"))
                 (:module "server"
                  :components ((:file "file-serving")
                               (:file "js-utils")
                               (:file "js-serving" :depends-on ("js-utils" "file-serving")))
                  :depends-on ("http")))))
  :depends-on (:metabang-bind
               :iterate
               :cl-def
               :defclass-star
               :computed-class
               :alexandria
               :anaphora
               :rfc2109
               :rfc2388-binary
               :net-telent-date ;; TODO get rid of this
               :bordeaux-threads
               :cffi
               :iolib
               :local-time
               :babel
               :babel-streams
               :contextl
               :parse-number
               :cl-fad
               :cl-yalog
               :cl-syntax-sugar
               :cl-l10n
               :cl-quasi-quote-xml
               :cl-quasi-quote-js
               :cl-delico
               :cl-dwim-util))

(defsystem* :wui-application-server
  :description "Extension to the basic HTTP server to become an HTTP application server for the world wide web."
  :long-description "Provides application, session, frame, action and entry point abstractions."
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Levente Mészáros <levente.meszaros@gmail.com>"
	       "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD (sans advertising clause)"
  :test-system :wui-application-server-test
  :components
  ((:module "src"
    :components ((:module "application"
                  :components ((:file "dojo")
                               (:file "variables")
                               (:file "session" :depends-on ("variables"))
                               (:file "frame" :depends-on ("variables"))
                               (:file "application" :depends-on ("variables" "dojo"))
                               (:file "entry-point" :depends-on ("variables"))
                               (:file "action" :depends-on ("variables")))))))
  :depends-on (:wui-http-server))

(defsystem* :wui-component-server
  :description "Extension to the HTTP application server to become an HTTP component based user interface server for the world wide web."
  :long-description "Provides various components, layouts, widgets, charts, books, model documentation components, meta components. Components have server and client side state and behavior."
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Levente Mészáros <levente.meszaros@gmail.com>"
	       "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD (sans advertising clause)"
  :test-system :wui-component-server-test
  :components
  ((:module "src"
    :components ((:module "util"
                  :components ((:file "csv")
                               (:file "book")
                               (:file "dictionary")
                               (:file "place")
                               #+sbcl(:file "object-size")))
                 (:module "component"
                  :components ((:module "api"
                                :components ((:file "api")
                                             (:file "mop")
                                             #+sbcl(:file "sbcl-ctor-kludge" :depends-on ("mop"))
                                             (:file "number" :depends-on ("api"))
                                             (:file "string" :depends-on ("api"))
                                             (:file "component" :depends-on ("api" "mop"))
                                             (:file "computed" :depends-on ("component"))
                                             (:file "interaction" :depends-on ("component"))
                                             (:file "response" :depends-on ("component"))
                                             (:file "debug" :depends-on ("component"))))
                               (:module "mixin"
                                :components ((:file "border")
                                             (:file "cell")
                                             (:file "cloneable")
                                             (:file "closable")
                                             (:file "column")
                                             (:file "command")
                                             (:file "command-bar")
                                             (:file "context-menu")
                                             (:file "content")
                                             (:file "draggable")
                                             (:file "editable")
                                             (:file "enableable")
                                             (:file "expandible")
                                             (:file "exportable")
                                             (:file "footer")
                                             (:file "header")
                                             (:file "initargs")
                                             (:file "layer")
                                             (:file "menu")
                                             (:file "menu-bar")
                                             (:file "mouse")
                                             (:file "page-navigation-bar")
                                             (:file "parent")
                                             (:file "refreshable")
                                             (:file "renderable")
                                             (:file "resizable")
                                             (:file "row")
                                             (:file "table")
                                             (:file "tooltip")
                                             (:file "top")
                                             (:file "tree")
                                             (:file "value")
                                             (:file "visibility")
                                             (:file "title" :depends-on ("refreshable"))
                                             (:file "id" :depends-on ("refreshable"))
                                             (:file "remote" :depends-on ("id"))
                                             (:file "style" :depends-on ("remote")))
                                :depends-on ("api"))
                               (:module "layout"
                                :components ((:file "layout")
                                             (:file "empty" :depends-on ("layout"))
                                             (:file "container" :depends-on ("layout"))
                                             (:file "list" :depends-on ("layout"))
                                             (:file "alternator" :depends-on ("layout"))
                                             (:file "cell" :depends-on ("layout"))
                                             (:file "row" :depends-on ("cell"))
                                             (:file "table" :depends-on ("row"))
                                             (:file "xy" :depends-on ("layout")))
                                :depends-on ("mixin"))
                               (:module "widget"
                                :components ((:file "widget")
                                             (:file "border")
                                             (:file "content")
                                             (:file "field")
                                             (:file "icon")
                                             (:file "image")
                                             (:file "inline")
                                             (:file "graph")
                                             (:file "message")
                                             (:file "path")
                                             (:file "splitter")
                                             (:file "title")
                                             (:file "tooltip")
                                             (:file "tool-bar")
                                             (:file "top" :depends-on ("message"))
                                             (:file "tree")
                                             (:file "tree-level")
                                             (:file "frame" :depends-on ("top"))
                                             (:file "command" :depends-on ("icon"))
                                             (:file "command-bar" :depends-on ("command"))
                                             (:file "menu" :depends-on ("command"))
                                             (:file "debug" :depends-on ("menu" "frame" "inline"))
                                             (:file "page-navigation-bar" :depends-on ("command"))
                                             (:file "expandible" :depends-on ("command"))
                                             (:file "help" :depends-on ("icon"))
                                             (:file "tab-container" :depends-on ("command-bar"))
                                             (:file "external-link")
                                             #+nil
                                             ((:file "panel" :depends-on ("message"))
                                              (:file "cell")
                                              (:file "row")
                                              (:file "column")
                                              (:file "table")
                                              (:file "node"))
                                             #+nil
                                             ((:file "alternator")
                                              (:file "authentication")
                                              (:file "expression")
                                              (:file "extended-table")
                                              (:file "field")
                                              (:file "file-up-and-download")
                                              (:file "frame-size-breakdown")
                                              (:file "internal-error")
                                              (:file "pivot-table")
                                              (:file "timestamp-range")
                                              (:file "wizard")))
                                :depends-on ("layout"))
                               (:module "chart"
                                :components ((:file "chart")
                                             (:file "column" :depends-on ("chart"))
                                             (:file "structure" :depends-on ("chart"))
                                             (:file "flow" :depends-on ("chart"))
                                             (:file "line" :depends-on ("chart"))
                                             (:file "pie" :depends-on ("chart"))
                                             (:file "radar" :depends-on ("chart"))
                                             (:file "stock" :depends-on ("chart"))
                                             (:file "xy" :depends-on ("chart")))
                                :depends-on ("widget"))
                               (:module "meta"
                                :components ((:file "maker")
                                             (:file "viewer")
                                             (:file "editor")
                                             (:file "inspector")
                                             (:file "filter")
                                             (:file "finder")
                                             (:file "selector")
                                             (:file "invoker"))
                                :depends-on ("api"))
                               (:module "book"
                                :components ((:file "book")
                                             (:file "toc" :depends-on ("book"))
                                             (:file "glossary" :depends-on ("book"))
                                             (:file "index" :depends-on ("book"))
                                             (:file "paragraph" :depends-on ("book"))
                                             (:file "chapter" :depends-on ("paragraph")))
                                :depends-on ("widget" "meta"))
                               (:module "model"
                                :components ((:file "model")
                                             #+nil
                                             (:file "class")
                                             #+nil
                                             (:file "slot")
                                             (:file "function")
                                             (:file "generic")
                                             (:file "package")
                                             (:file "file")
                                             (:file "name")
                                             (:file "dictionary")
                                             (:file "system")
                                             (:file "module")
                                             (:file "type")
                                             (:file "variable"))
                                :depends-on ("widget" "meta"))
                               (:module "primitive"
                                :components ((:file "primitive")
                                             (:file "abstract" :depends-on ("primitive"))
                                             (:file "viewer" :depends-on ("primitive"))
                                             (:file "editor" :depends-on ("primitive"))
                                             (:file "inspector" :depends-on ("primitive"))
                                             (:file "filter" :depends-on ("primitive"))
                                             (:file "maker" :depends-on ("primitive")))
                                :depends-on ("widget" "meta"))
                               (:module "place"
                                :components (#+nil
                                             ((:file "place")
                                              (:file "filter" :depends-on ("place"))
                                              (:file "inspector" :depends-on ("place"))
                                              (:file "maker" :depends-on ("place"))))
                                :depends-on ("widget" "meta"))
                               (:module "object"
                                :components ((:file "xxx")
                                             #+nil
                                             ((:file "object")
                                              (:file "filter" :depends-on ("object"))
                                              (:file "inspector" :depends-on ("object"))
                                              (:file "list-inspector" :depends-on ("object"))
                                              (:file "maker" :depends-on ("object"))
                                              (:file "manager" :depends-on ("object"))
                                              (:file "pivot-table" :depends-on ("object"))
                                              (:file "process" :depends-on ("object"))
                                              (:file "reference" :depends-on ("object"))
                                              (:file "tree-filter" :depends-on ("object"))
                                              (:file "tree-inspector" :depends-on ("object"))))
                                :depends-on ("primitive" "place"))
                               (:module "shortcut"
                                :components ((:file "layout")
                                             (:file "widget"))
                                :depends-on ("object")))
                  :depends-on ("util")))))
  :depends-on (:contextl :wui-application-server))

(defsystem* :wui
  :description "WUI with all its extensions."
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Levente Mészáros <levente.meszaros@gmail.com>"
	       "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD (sans advertising clause)"
  :test-system :wui-test
  :depends-on (:wui-component-server))

#+nil
(defmethod perform :around ((o t) (system wui-system))
  (progv
      (list
       (read-from-string "cl-delico:*call/cc-returns*")
       ;; If you want the walker to warn about undefined variables and
       ;; functions change this to T. Since this code "breaks" (sort of)
       ;; wui loading with ASDF on SBCL we leave it off by default.
       (read-from-string "cl-walker::*warn-for-undefined-references*"))
      (list nil nil)
    (call-next-method)))

;;;;;;
;;; Test systems

(defsystem* :wui-http-server-test
  :setup-readtable-function "hu.dwim.wui.test::setup-readtable"
  :components
  ((:module "test"
    :components ((:file "package")
                 (:file "environment" :depends-on ("package"))
                 (:file "http" :depends-on ("environment")))))
  :depends-on (:wui-http-server :stefil :drakma))

(defsystem* :wui-application-server-test
  :setup-readtable-function "hu.dwim.wui.test::setup-readtable"
  :components
  ((:module "test"
    :components ((:file "application"))))
  :depends-on (:wui-application-server :wui-http-server-test))

(defsystem* :wui-component-server-test
  :setup-readtable-function "hu.dwim.wui.test::setup-readtable"
  :components
  ((:module "test"
    :components ((:file "component"))))
  :depends-on (:wui-component-server :wui-application-server-test :wui-and-informatimago))

(defsystem* :wui-test
  :setup-readtable-function "hu.dwim.wui.test::setup-readtable"
  :depends-on (:wui :wui-component-server-test))

(defmethod perform ((op test-op) (system wui-system))
  (format *debug-io* "~%*** Testing ~A using the ~A test system~%~%" system (test-system-of system))
  (setf *load-as-production-p* nil)
  (operate 'load-op (test-system-of system))
  (in-package :wui.test)
  (declaim (optimize (debug 3)))
  (warn "Issued a (declaim (optimize (debug 3))) for easy C-c C-c'ing")
  ;; KLUDGE ASDF wraps everything in a WITH-COMPILATION-UNIT which eventually prevents starting the
  ;; tests on SBCL due to The Big Compiler Lock.
  (eval (read-from-string "(bordeaux-threads:make-thread
                             (lambda ()
                               (stefil:funcall-test-with-feedback-message 'wui.test:test)))"))
  (values))

(defmethod operation-done-p ((op test-op) (system wui-system))
  nil)

;;;;;;
;;; WUI integration with other systems

(defsystem* wui-and-cl-perec
  :depends-on (:wui
               :cl-l10n
               :cl-perec
               :dwim-meta-model)
  :components
  ((:module "src/integration/cl-perec"
    :components ((:file "kludge")
                 (:file "factory")
                 (:file "l10n")
                 (:file "menu")
                 (:file "place")
                 (:file "reference")
                 (:file "graph")
                 (:file "editable")
                 (:file "exportable")
                 (:file "expression")
                 (:file "alternator")
                 (:file "object-component")
                 (:file "object-inspector")
                 (:file "object-list-inspector")
                 (:file "object-tree-inspector")
                 (:file "object-maker")
                 (:file "object-filter")
                 (:file "process")
                 (:file "dimensional")
                 (:file "query-expression")))))

(defmethod perform ((op load-op) (system (eql (find-system :wui-and-cl-perec))))
  (pushnew :wui-and-cl-perec *features*))

(defsystem* wui-and-cl-typesetting
  :depends-on (:wui :cl-typesetting)
  :components
  ((:module "src"
    :components ((:module "integration"
                  :components ((:file "cl-typesetting")))))))

(defsystem* wui-and-cl-serializer
  :depends-on (:wui :cl-serializer :cl-perec)
  :components
  ((:module "src"
    :components ((:module "integration"
                  :components ((:file "cl-serializer")))))))

(defsystem* wui-and-informatimago
  :depends-on (:wui)
  :components
  ((:module "src"
    :components ((:module "integration"
                  :components ((:module "informatimago"
                                :components ((:file "source-form")
                                             (:file "reader" :depends-on ("source-form"))
                                             (:file "source-text" :depends-on ("reader"))))))
                 (:module "component"
                  :components ((:module "model"
                                :components ((:file "form"))))
                  :depends-on ("integration"))))))
