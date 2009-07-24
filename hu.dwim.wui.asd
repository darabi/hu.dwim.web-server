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
           #:project-relative-pathname)

  (:use :common-lisp
        :asdf
        :cl-syntax-sugar
        :alexandria))

(in-package :hu.dwim.wui.system)

(defun project-relative-pathname (path)
  (merge-pathnames path (component-pathname (find-system :hu.dwim.wui))))

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

(defsystem* :hu.dwim.wui.http
  :description "Basic HTTP server to build user interfaces for the world wide web."
  :long-description "Provides error handling, compression, static file serving, quasi quoted JavaScript and quasi quoted XML serving."
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Levente Mészáros <levente.meszaros@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD (sans advertising clause)"
  :test-system :hu.dwim.wui.http.test
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "duplicates" :depends-on ("package"))
                 (:file "configuration" :depends-on ("package"))
                 (:file "logging" :depends-on ("package"))
                 (:module "util"
                  :components ((:file "l10n" :depends-on ("utils"))
                               (:file "timer")
                               (:file "utils")
                               (:file "zlib"))
                  :depends-on ("logging" "configuration" "duplicates"))
                 (:module "http"
                  :components ((:file "accept-headers" :depends-on ("variables"))
                               (:file "brokers" :depends-on ("server"))
                               (:file "conditions" :depends-on ("variables"))
                               (:file "error-handling" :depends-on ("variables" "utils"))
                               (:file "request-parsing" :depends-on ("request-response" "uri"))
                               (:file "request-response" :depends-on ("variables" "utils"))
                               (:file "server" :depends-on ("request-parsing"))
                               (:file "uri" :depends-on ("variables"))
                               (:file "utils" :depends-on ("variables"))
                               (:file "variables"))
                  :depends-on ("logging" "util"))
                 (:module "server"
                  :components ((:file "file-serving")
                               (:file "js-serving" :depends-on ("js-utils" "file-serving"))
                               (:file "js-utils"))
                  :depends-on ("http")))))
  :depends-on (:alexandria
               :anaphora
               :babel
               :babel-streams
               :bordeaux-threads
               :cffi
               :cl-def
               :cl-delico
               :cl-fad
               :cl-l10n
               :cl-quasi-quote-js
               :cl-quasi-quote-xml
               :cl-syntax-sugar
               :cl-yalog
               :computed-class
               :contextl
               :defclass-star
               :hu.dwim.util
               :iolib
               :iterate
               :local-time
               :metabang-bind
               :net-telent-date ;; TODO get rid of this
               :parse-number
               :rfc2109
               :rfc2388-binary))

(defsystem* :hu.dwim.wui.application
  :description "Extension to the basic HTTP server to become an HTTP application server for the world wide web."
  :long-description "Provides application, session, frame, action and entry point abstractions."
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Levente Mészáros <levente.meszaros@gmail.com>"
	       "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD (sans advertising clause)"
  :test-system :hu.dwim.wui.application.test
  :components
  ((:module "src"
    :components ((:module "application"
                  :components ((:file "action" :depends-on ("variables" "application" "frame"))
                               (:file "application" :depends-on ("variables" "dojo" "session" "frame"))
                               (:file "dojo")
                               (:file "entry-point" :depends-on ("variables"))
                               (:file "frame" :depends-on ("variables" "session"))
                               (:file "session" :depends-on ("variables"))
                               (:file "variables"))))))
  :depends-on (:hu.dwim.wui.http))

(defsystem* :hu.dwim.wui.component
  :description "Extension to the HTTP application server to become an HTTP component based user interface server for the world wide web."
  :long-description "Provides various components, layouts, widgets, charts, books, model documentation components, meta components. Components have server and client side state and behavior."
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Levente Mészáros <levente.meszaros@gmail.com>"
	       "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD (sans advertising clause)"
  :test-system :hu.dwim.wui.component.test
  :components
  ((:module "src"
    :components ((:module "util"
                  :components ((:file "book")
                               (:file "csv")
                               (:file "dictionary")
                               #+sbcl(:file "object-size")
                               (:file "place")))
                 (:module "component"
                  :components ((:module "api"
                                :components ((:file "api")
                                             (:file "component" :depends-on ("api" "mop"))
                                             (:file "computed" :depends-on ("component"))
                                             (:file "debug" :depends-on ("component"))
                                             (:file "interaction" :depends-on ("component"))
                                             (:file "mop")
                                             (:file "number" :depends-on ("api"))
                                             (:file "response" :depends-on ("component"))
                                             #+sbcl(:file "sbcl-ctor-kludge" :depends-on ("mop"))
                                             (:file "string" :depends-on ("api"))))
                               (:module "mixin"
                                :components ((:file "border")
                                             (:file "cell" :depends-on ("column"))
                                             (:file "cloneable")
                                             (:file "closable")
                                             (:file "collapsible")
                                             (:file "column")
                                             (:file "command")
                                             (:file "command-bar")
                                             (:file "content")
                                             (:file "context-menu")
                                             (:file "disableable")
                                             (:file "draggable")
                                             (:file "editable")
                                             (:file "exportable")
                                             (:file "footer")
                                             (:file "header")
                                             (:file "hideable")
                                             (:file "id" :depends-on ("refreshable"))
                                             (:file "initargs")
                                             (:file "layer")
                                             (:file "menu")
                                             (:file "menu-bar")
                                             (:file "mouse")
                                             (:file "node" :depends-on ("tree"))
                                             (:file "page-navigation-bar")
                                             (:file "parent")
                                             (:file "refreshable")
                                             (:file "remote" :depends-on ("id"))
                                             (:file "renderable")
                                             (:file "resizable")
                                             (:file "row")
                                             (:file "scrollable")
                                             (:file "selectable")
                                             (:file "style" :depends-on ("remote"))
                                             (:file "table")
                                             (:file "title" :depends-on ("refreshable"))
                                             (:file "tooltip")
                                             (:file "top")
                                             (:file "tree")
                                             (:file "value"))
                                :depends-on ("api"))
                               (:module "layout"
                                :components ((:file "alternator" :depends-on ("layout"))
                                             (:file "cell" :depends-on ("layout"))
                                             (:file "container" :depends-on ("layout"))
                                             (:file "empty" :depends-on ("layout"))
                                             (:file "flow" :depends-on ("layout"))
                                             (:file "layout")
                                             (:file "list" :depends-on ("layout"))
                                             (:file "node" :depends-on ("layout"))
                                             (:file "row" :depends-on ("cell"))
                                             (:file "table" :depends-on ("row"))
                                             (:file "tree" :depends-on ("node"))
                                             (:file "treeble" :depends-on ("row"))
                                             (:file "xy" :depends-on ("layout")))
                                :depends-on ("mixin"))
                               (:module "widget"
                                :components ((:file "alternator" :depends-on ("reference" "command" "menu"))
                                             (:file "border")
                                             (:file "button")
                                             (:file "cell" :depends-on ("table" "row" "column"))
                                             (:file "collapsible" :depends-on ("command"))
                                             (:file "column")
                                             (:file "command" :depends-on ("icon"))
                                             (:file "command-bar" :depends-on ("command"))
                                             (:file "content")
                                             (:file "debug" :depends-on ("menu" "frame" "inline" "replace-target"))
                                             (:file "demo" :depends-on ("tab-container" "content"))
                                             (:file "element" :depends-on ("command"))
                                             (:file "external-link")
                                             (:file "field")
                                             (:file "frame" :depends-on ("top"))
                                             (:file "help" :depends-on ("icon"))
                                             (:file "icon")
                                             (:file "image")
                                             (:file "inline")
                                             (:file "internal-error" :depends-on ("message" "command-bar" "command"))
                                             (:file "list")
                                             (:file "menu" :depends-on ("command"))
                                             (:file "message")
                                             (:file "name-value")
                                             (:file "node")
                                             (:file "nodrow" :depends-on ("treeble" "column" "cell"))
                                             (:file "page-navigation-bar" :depends-on ("command"))
                                             (:file "panel" :depends-on ("message"))
                                             (:file "path")
                                             (:file "reference")
                                             (:file "replace-target" :depends-on ("command"))
                                             (:file "row" :depends-on ("table"))
                                             (:file "scroll")
                                             (:file "scroll-bar")
                                             (:file "splitter")
                                             (:file "tab-container" :depends-on ("command-bar"))
                                             (:file "table")
                                             (:file "title")
                                             (:file "tool-bar")
                                             (:file "tooltip")
                                             (:file "top" :depends-on ("message"))
                                             (:file "tree")
                                             (:file "tree-level")
                                             (:file "treeble")
                                             (:file "widget")
                                             #+nil
                                             ((:file "authentication")
                                              (:file "expression")
                                              (:file "extended-table")
                                              (:file "file-up-and-download")
                                              (:file "frame-size-breakdown")
                                              (:file "pivot-table")
                                              (:file "timestamp-range")
                                              (:file "wizard")))
                                :depends-on ("layout"))
                               (:module "chart"
                                :components ((:file "chart")
                                             (:file "column" :depends-on ("chart"))
                                             (:file "flow" :depends-on ("chart"))
                                             (:file "line" :depends-on ("chart"))
                                             (:file "pie" :depends-on ("chart"))
                                             (:file "radar" :depends-on ("chart"))
                                             (:file "scatter" :depends-on ("chart"))
                                             (:file "stock" :depends-on ("chart"))
                                             (:file "structure" :depends-on ("chart")))
                                :depends-on ("widget"))
                               (:module "meta"
                                :components ((:file "editor")
                                             (:file "filter")
                                             (:file "finder")
                                             (:file "inspector")
                                             (:file "invoker")
                                             (:file "maker")
                                             (:file "selector")
                                             (:file "viewer")
                                             (:file "xxx"))
                                :depends-on ("api"))
                               (:module "book"
                                :components ((:file "book")
                                             (:file "chapter" :depends-on ("paragraph"))
                                             (:file "glossary" :depends-on ("book"))
                                             (:file "index" :depends-on ("book"))
                                             (:file "paragraph" :depends-on ("book"))
                                             (:file "toc" :depends-on ("book")))
                                :depends-on ("widget" "meta"))
                               (:module "model"
                                :components ((:file "class")
                                             (:file "dictionary")
                                             (:file "file")
                                             (:file "function")
                                             (:file "generic")
                                             (:file "method")
                                             (:file "model")
                                             (:file "module")
                                             (:file "name")
                                             (:file "package")
                                             (:file "pathname")
                                             (:file "repl")
                                             (:file "slot")
                                             (:file "system")
                                             (:file "type")
                                             (:file "variable"))
                                :depends-on ("widget" "meta"))
                               (:module "primitive"
                                :components ((:file "abstract" :depends-on ("primitive"))
                                             (:file "editor" :depends-on ("primitive"))
                                             (:file "filter" :depends-on ("primitive"))
                                             (:file "inspector" :depends-on ("primitive"))
                                             (:file "maker" :depends-on ("primitive"))
                                             (:file "primitive")
                                             (:file "viewer" :depends-on ("primitive")))
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
                                :depends-on ("object"))
                               ;; KLUDGE: kill this
                               (:file "xxx" :depends-on ("shortcut")))
                  :depends-on ("util")))))
  :depends-on (:contextl
               :hu.dwim.wui.application))

(defsystem* :hu.dwim.wui
  :description "WUI with all its extensions."
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Levente Mészáros <levente.meszaros@gmail.com>"
	       "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD (sans advertising clause)"
  :test-system :hu.dwim.wui.test
  :depends-on (:hu.dwim.wui.component))

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

(defsystem* :hu.dwim.wui.http.test
  :setup-readtable-function "hu.dwim.wui.test::setup-readtable"
  :components
  ((:module "test"
    :components ((:file "package")
                 (:file "environment" :depends-on ("package"))
                 (:file "http" :depends-on ("environment")))))
  :depends-on (:drakma
               :hu.dwim.wui.http
               :stefil))

(defsystem* :hu.dwim.wui.application.test
  :setup-readtable-function "hu.dwim.wui.test::setup-readtable"
  :components
  ((:module "test"
    :components ((:file "application"))))
  :depends-on (:hu.dwim.wui.application
               :hu.dwim.wui.http.test))

(defsystem* :hu.dwim.wui.component.test
  :setup-readtable-function "hu.dwim.wui.test::setup-readtable"
  :components
  ((:module "test"
    :components ((:file "component"))))
  :depends-on (:hu.dwim.wui.application.test
               :hu.dwim.wui.component
               :hu.dwim.wui&informatimago
               :hu.dwim.wui&cl-graph))

(defsystem* :hu.dwim.wui.test
  :setup-readtable-function "hu.dwim.wui.test::setup-readtable"
  :depends-on (:hu.dwim.wui
               :hu.dwim.wui.component.test))

(defmethod perform ((op test-op) (system wui-system))
  (format *debug-io* "~%*** Testing ~A using the ~A test system~%~%" system (test-system-of system))
  (setf *load-as-production-p* nil)
  (operate 'load-op (test-system-of system))
  (in-package :hu.dwim.wui.test)
  (declaim (optimize (debug 3)))
  (let ((*package* (find-package :hu.dwim.wui)))
    (eval (read-from-string "(progn
                               ;; set dojo to the latest available
                               (setf *dojo-directory-name* (find-latest-dojo-directory-name (asdf:system-relative-pathname :hu.dwim.wui \"wwwroot/\")))
                               (setf (log-level 'wui) +debug+)
                               (setf *debug-on-error* t)
                               ;; KLUDGE ASDF wraps everything in a WITH-COMPILATION-UNIT which eventually prevents starting the
                               ;; tests on SBCL due to The Big Compiler Lock, so we spawn here a thread to run the tests.
                               (bordeaux-threads:make-thread
                                 (lambda ()
                                   (stefil:funcall-test-with-feedback-message 'wui.test:test))))")))
  (warn "Issued a (declaim (optimize (debug 3))) for easy C-c C-c'ing; set WUI log level to +debug+; enabled server-side debugging")
  (values))

(defmethod operation-done-p ((op test-op) (system wui-system))
  nil)

;;;;;;
;;; WUI integration with other systems

(defsystem* hu.dwim.wui&cl-perec
  :depends-on (:cl-l10n
               :cl-perec
               :dwim-meta-model
               :hu.dwim.wui)
  :components
  ((:module "src/integration/cl-perec"
    :components (#+nil
                 ((:file "kludge")
                  (:file "factory")
                  (:file "l10n")
                  (:file "menu")
                  (:file "place")
                  (:file "reference")
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
                  (:file "query-expression"))))))

(defmethod perform ((op load-op) (system (eql (find-system :hu.dwim.wui&cl-perec))))
  (pushnew :hu.dwim.wui&cl-perec *features*))

(defsystem* :hu.dwim.wui&cl-typesetting
  :depends-on (:cl-typesetting
               :hu.dwim.wui)
  :components
  ((:module "src"
    :components ((:module "integration"
                  :components ((:file "cl-typesetting")))))))

(defsystem* :hu.dwim.wui&cl-serializer
  :depends-on (:cl-serializer
               :cl-perec
               :hu.dwim.wui)
  :components
  ((:module "src"
    :components ((:module "integration"
                  :components ((:file "cl-serializer")))))))

(defsystem* :hu.dwim.wui&informatimago
  :depends-on (:hu.dwim.wui)
  :components
  ((:module "src"
    :components ((:module "integration"
                  :components ((:module "informatimago"
                                :components ((:file "reader" :depends-on ("source-form"))
                                             (:file "source-form")
                                             (:file "source-text" :depends-on ("reader"))
                                             (:file "syntax-sugar")))))
                 (:module "component"
                  :components ((:module "model"
                                :components ((:file "form"))))
                  :depends-on ("integration"))))))

(defsystem* :hu.dwim.wui&cl-graph
  :depends-on (:cl-graph
               :hu.dwim.wui)
  :components
  ((:module "src"
    :components ((:module "component"
                  :components ((:module "widget"
                                :components ((:file "graph")))))))))

(defsystem* :hu.dwim.wui&stefil
  :depends-on (:stefil
               :hu.dwim.wui)
  :components
  ((:module "src"
    :components ((:module "component"
                  :components ((:module "model"
                                :components ((:file "test")))))))))
