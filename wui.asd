;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(cl:in-package :cl-user)

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
  (:export
   #:*load-as-production-p*
   #:project-relative-pathname
   )
  (:use
   :common-lisp
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
    (call-next-method)))

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

(defsystem* :wui-core
  :description "Core features of WUI"
  :long-description "Contains the base features essential for a useful Read Eval Render Loop (RERL)."
  :author "Attila Lendva <attila.lendvai@gmail.com>"
  :licence "BSD (sans advertising clause)"
  :test-system :wui-test
  :components
  ((:module :src
    :components ((:file "packages")
                 (:file "duplicates" :depends-on ("packages"))
                 (:file "configuration" :depends-on ("packages" "readers"))
                 (:file "variables" :depends-on ("packages" "duplicates" "configuration"))
                 (:file "readers" :depends-on ("packages" "duplicates"))
                 (:file "utils" :depends-on ("packages" "duplicates" "readers" "variables" "configuration"))
                 (:file "loggers" :depends-on ("packages" "configuration" "variables" "utils"))
                 (:module "http"
                  :serial t
                  :components ((:file "http-utils")
                               (:file "conditions")
                               (:file "uri")
                               (:file "accept-headers")
                               (:file "request-response")
                               (:file "error-handling")
                               (:file "request-parsing")
                               (:file "server")
                               (:file "brokers")
                               (:file "file-serving"))
                  :depends-on ("packages" "loggers" "utils")))))
  :depends-on (:iterate
               :metabang-bind
               :cl-def
               :defclass-star
               :alexandria
               :rfc2109
               :net-telent-date
               :cl-fad
               :bordeaux-threads
               :osicat
               :iolib
               :local-time
               :babel
               :babel-streams
               :cl-yalog
               :cl-syntax-sugar
               :cl-quasi-quote-xml
               :cl-quasi-quote-js
               :cl-delico
               ))

(defsystem* :wui
  :description "WUI with all its extensions"
  :long-description "WUI and all its featues loaded"
  :author "Attila Lendva <attila.lendvai@gmail.com>"
  :licence "BSD (sans advertising clause)"
  :test-system :wui-test
  :components
  ((:module :src
    :components ((:module "application"
                  :serial t
                  :components ((:file "session")
                               (:file "frame")
                               (:file "application")
                               (:file "entry-point")
                               (:file "action")
                               (:file "error-handlers")))
                 (:module "component"
                  :components ((:file "mop" )
                               (:file "syntax")
                               (:file "component" :depends-on ("mop" "syntax"))
                               (:file "api" :depends-on ("component"))
                               (:file "place" :depends-on ("component"))
                               (:file "icon" :depends-on ("component"))
                               (:file "command" :depends-on ("icon" "place"))
                               (:file "misc" :depends-on ("component"))
                               (:file "list" :depends-on ("component"))
                               (:file "table" :depends-on ("component"))
                               (:file "tree" :depends-on ("component"))
                               (:file "menu" :depends-on ("command"))
                               (:file "reference" :depends-on ("command"))
                               (:file "editable" :depends-on ("command"))
                               (:file "place-component" :depends-on ("editable" "api"))
                               (:file "atomic" :depends-on ("editable"))
                               (:file "process" :depends-on ("command"))
                               (:file "alternator" :depends-on ("reference" "command"))
                               (:file "class" :depends-on ("alternator" "reference"))
                               (:file "object-detail" :depends-on ("alternator" "reference"))
                               (:file "object-maker" :depends-on ("object-detail"))
                               (:file "object-table" :depends-on ("object-detail" "table"))
                               (:file "filter" :depends-on ("object-detail"))
                               (:file "parser" :depends-on ("atomic" "reference" "object-detail" "filter" "class" "process")))
                  :depends-on ("application")))))
  :depends-on (:wui-core
               :trivial-garbage
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

(defsystem* :wui-test
  :setup-readtable-function "hu.dwim.wui-test::setup-readtable"
  :components ((:module :test
                :serial t
                :components
                ((:file "package")
                 (:file "test-environment" :depends-on ("package"))
                 (:file "server")
                 (:file "component")
                 (:file "application"))))
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

;;; Integration with other systems

#+nil
(defsystem-connection wui-and-contextl
  :requires (:wui :contextl)
  :components ((:module :src
                        :components ((:file "contextl-integration")))))

