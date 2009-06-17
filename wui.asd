;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :cl-user)

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

(defsystem* :wui-server
  :description "Basic HTTP server to build user interfaces for the world wide web."
  :long-description "Provides error handling, compression, static file serving, quasi quoted JavaScript and quasi quoted XML serving."
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Levente Mészáros <levente.meszaros@gmail.com>"
	       "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD (sans advertising clause)"
  :test-system :wui-test
  :components
  ((:module "src"
    :components ((:file "zlib" :pathname "util/zlib.lisp")
                 (:file "packages" :depends-on ("zlib"))
                 (:file "duplicates" :depends-on ("packages"))
                 (:file "configuration" :depends-on ("packages"))
                 (:file "variables" :depends-on ("packages" "duplicates" "configuration"))
                 (:file "utils" :depends-on ("packages" "duplicates" "variables" "configuration"))
                 (:file "js-utils" :depends-on ("utils"))
                 (:file "file-serving" :depends-on ("utils" "http"))
                 (:file "js-serving" :depends-on ("utils" "js-utils" "file-serving"))
                 (:file "loggers" :depends-on ("packages" "configuration" "variables" "utils"))
                 (:module "http"
                  :serial t
                  :components ((:file "uri")
                               (:file "http-utils")
                               (:file "conditions")
                               (:file "accept-headers")
                               (:file "request-response")
                               (:file "error-handling")
                               (:file "request-parsing")
                               (:file "server")
                               (:file "brokers"))
                  :depends-on ("packages" "loggers" "utils")))))
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
               :parse-number
               :cl-fad
               :cl-yalog
               :cl-syntax-sugar
               :cl-l10n
               :cl-quasi-quote-xml
               :cl-quasi-quote-js
               :cl-delico
               ))

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
  :test-system :wui-test
  :components
  ((:module "src"
    :serial t
    :components ((:file "l10n")
                 (:file "dojo")
                 (:module "util"
                  :components ((:file "timer")
                               #+sbcl(:file "object-size")))
                 (:module "application"
                  :serial t
                  :depends-on ("dojo")
                  :components ((:file "api")
                               (:file "session")
                               (:file "frame")
                               (:file "application")
                               (:file "entry-point")
                               (:file "action"))))))
  :depends-on (:wui-server
               ))

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
  :test-system :wui-test
  :components
  ((:module "src"
    :serial t
    :components ((:module "component"
                  :components ((:module "api"
                                :components ())
                               (:module "book"
                                :components ())
                               (:module "chart"
                                :components ())
                               (:module "layout"
                                :components ())
                               (:module "mixin"
                                :components ())
                               (:module "model"
                                :components ())
                               (:module "object"
                                :components ())
                               (:module "place"
                                :components ())
                               (:module "primitive"
                                :components ())
                               (:module "util"
                                :components ())
                               (:module "widget"
                                :components ()))
                  #+nil
                  ((:file "error-handlers")
                   (:file "api")
                   (:file "mop")
                   #+sbcl(:file "sbcl-ctor-kludge" :depends-on ("mop"))
                   (:file "component" :depends-on ("api" "mop"))
                   (:file "tooltip" :depends-on ("component"))
                   (:file "icon" :depends-on ("tooltip"))
                   (:file "menu" :depends-on ("icon"))
                   (:file "debug" :depends-on ("menu"))
                   (:file "response" :depends-on ("component"))
                   (:file "place" :depends-on ("component"))
                   (:file "parent" :depends-on ("component"))
                   (:file "renderable" :depends-on ("component"))
                   (:file "id" :depends-on ("component"))
                   (:file "command" :depends-on ("component" "response"))
                   (:file "refreshable" :depends-on ("command" "computed"))
                   (:file "closable" :depends-on ("command"))
                   (:file "user-message" :depends-on ("closable"))
                   (:file "computed" :depends-on ("component"))
                   (:file "debug" :depends-on ("menu"))
                   (:file "cloneable" :depends-on ("command"))
                   (:file "visible" :depends-on ("command"))
                   (:file "expandible" :depends-on ("command"))
                   (:file "enabled" :depends-on ("component"))
                   (:file "value" :depends-on ("component"))
                   (:file "style" :depends-on ("component"))
                   (:file "content" :depends-on ("component"))
                   (:file "command" :depends-on ("component"))
                   (:file "id" :depends-on ("component"))
                   (:file "remote" :depends-on ("id"))
                   (:file "style" :depends-on ("remote"))
                   (:file "place" :depends-on ("component"))
                   (:file "list" :depends-on ("component"))
                   (:file "menu" :depends-on ("component"))
                   (:file "title" :depends-on ("component"))
                   (:file "panel" :depends-on ("component"))
                   (:file "image" :depends-on ("component"))
                   (:file "factory" :depends-on ("component"))
                   (:file "place" :depends-on ("component"))
                   (:file "remote" :depends-on ("component"))
                   (:file "title" :depends-on ("component"))
                   (:file "help" :depends-on ("misc"))
                   (:file "command" :depends-on ("icon" "place" "misc"))
                   (:file "exportable" :depends-on ("command" "object-component"))
                   (:file "authentication" :depends-on ("command"))
                   (:file "frame" :depends-on ("debug"))
                   #+sbcl(:file "frame-size-breakdown" :depends-on ("component"))
                   (:file "file-up-and-download" :depends-on ("component"))
                   (:file "timestamp-range" :depends-on ("component"))
                   (:file "list" :depends-on ("component" "misc"))
                   (:file "table" :depends-on ("component"))
                   (:file "tab-container" :depends-on ("icon"))
                   (:file "extended-table" :depends-on ("command"))
                   (:file "pivot-table" :depends-on ("object-inspector" "extended-table" "menu" "icon"))
                   (:file "tree" :depends-on ("table"))
                   (:file "chart" :depends-on ("component"))
                   (:file "column-chart" :depends-on ("chart"))
                   (:file "line-chart" :depends-on ("chart"))
                   (:file "pie-chart" :depends-on ("chart"))
                   (:file "radar-chart" :depends-on ("chart"))
                   (:file "stock-chart" :depends-on ("chart"))
                   (:file "xy-chart" :depends-on ("chart"))
                   (:file "menu" :depends-on ("command" "misc"))
                   (:file "reference" :depends-on ("command"))
                   (:file "editable" :depends-on ("command" "object-component"))
                   (:file "field")
                   (:file "expression" :depends-on ("icon"))
                   (:file "primitive-component" :depends-on ("misc" "field" "place" "command" "object-component"))
                   (:file "primitive-maker" :depends-on ("primitive-component"))
                   (:file "primitive-inspector" :depends-on ("primitive-component"))
                   (:file "primitive-filter" :depends-on ("primitive-component"))
                   (:file "place-component" :depends-on ("place" "editable" "factory" "object-list-inspector" "object-component" "primitive-filter"))
                   (:file "user-message" :depends-on ("component" "icon" "command"))
                   (:file "wizard" :depends-on ("component"))
                   (:file "alternator" :depends-on ("reference" "command" "editable" "misc" "primitive-component"))
                   (:file "class" :depends-on ("object-component" "alternator" "reference" "table"))
                   (:file "object-component" :depends-on ("command" "title" "misc" "menu"))
                   (:file "object-manager" :depends-on ("object-component" "tab-container"))
                   (:file "object-inspector" :depends-on ("place-component" "object-component" "alternator" "reference"))
                   (:file "object-maker" :depends-on ("primitive-component" "object-component" "alternator"))
                   (:file "object-list-inspector" :depends-on ("object-maker" "object-component" "alternator" "reference" "table"))
                   (:file "object-pivot-table" :depends-on ("pivot-table" "object-list-inspector"))
                   (:file "object-list-aggregator" :depends-on ("object-component"))
                   (:file "object-tree-inspector" :depends-on ("object-component" "alternator" "reference" "tree"))
                   (:file "process" :depends-on ("command" "object-maker" "object-list-inspector" "object-tree-inspector"))
                   (:file "object-filter" :depends-on ("place-component" "object-component" "object-inspector" "primitive-filter"))
                   (:file "object-tree-filter" :depends-on ("object-filter")))
                  :depends-on ("application" "dojo")))))
  :depends-on (:wui-application-server
               ))

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
  :depends-on (:wui-component-server
               ))

#+nil
(defmethod perform :around ((o t) (system wui-system))
  (progv
      (list
       (read-from-string "cl-delico:*call/cc-returns*")
       ;; If you want the walker to warn about undefined variables and
       ;; functions change this to T. Since this code "breaks" (sort of)
       ;; loading wui with ASDF on SBCL we leave it off by default.
       (read-from-string "cl-walker::*warn-for-undefined-references*"))
      (list nil nil)
    (call-next-method)))

;;;;;;
;;; Test system

(defsystem* :wui-test
  :setup-readtable-function "hu.dwim.wui-test::setup-readtable"
  :components
  ((:module "test"
    :serial t
    :components ((:file "package")
                 (:file "environment" :depends-on ("package"))
                 (:file "server")
                 (:file "application")
                 (:file "wudemo-application"))))
  :depends-on (:wui :stefil :drakma))

(defmethod perform ((op test-op) (system wui-system))
  (format *debug-io* "~%*** Testing ~A using the ~A test system~%~%" system (test-system-of system))
  (setf *load-as-production-p* nil)
  (operate 'load-op (test-system-of system))
  (in-package :wui-test)
  (declaim (optimize (debug 3)))
  (warn "Issued a (declaim (optimize (debug 3))) for easy C-c C-c'ing")
  ;; KLUDGE ASDF wraps everything in a WITH-COMPILATION-UNIT which eventually prevents starting the
  ;; tests on SBCL due to The Big Compiler Lock.
  (eval (read-from-string "(bordeaux-threads:make-thread
                             (lambda ()
                               (stefil:funcall-test-with-feedback-message 'wui-test:test)))"))
  (values))

(defmethod operation-done-p ((op test-op) (system wui-system))
  nil)

;;;;;;
;;; Integration with other systems

(defsystem* wui-and-cl-perec
  :depends-on (:wui
               :cl-perec
               :cl-l10n
               :dwim-meta-model
               )
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
            :components
            ((:module "integration"
                      :components ((:file "cl-typesetting")))))))

(defsystem* wui-and-cl-serializer
  :depends-on (:wui :cl-serializer :cl-perec)
  :components
  ((:module "src"
            :components
            ((:module "integration"
                      :components ((:file "cl-serializer")))))))
