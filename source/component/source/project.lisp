;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; project/inspector

(def (component e) project/inspector (t/inspector)
  ())

(def (macro e) project/inspector (project &rest args &key &allow-other-keys)
  `(make-instance 'project/inspector ,project ,@args))

(def layered-method find-inspector-type-for-prototype ((prototype project))
  'project/inspector)

(def layered-method make-alternatives ((component project/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'project/detail/inspector :component-value value)
         (call-next-method)))

(def method localized-instance-name ((project project))
  (name-of project))

;;;;;;
;;; project/detail/inspector

(def (component e) project/detail/inspector (inspector/style t/detail/presentation tab-container/widget)
  ())

(def layered-method refresh-component :before ((self project/detail/inspector))
  (bind (((:slots hu.dwim.wui::tab-pages component-value) self))
    (setf hu.dwim.wui::tab-pages (make-project-tab-pages self component-value))))

(def (generic e) make-project-tab-pages (component project)
  (:method ((component project/detail/inspector) (project project))
    (list (tab-page/widget (:selector (icon switch-to-tab-page :label "Description"))
            (make-instance 'project/description/inspector :component-value project))
          (tab-page/widget (:selector (icon switch-to-tab-page :label "Content"))
            (make-instance 'project/content/inspector :component-value project))
          (tab-page/widget (:selector (icon switch-to-tab-page :label "Repository"))
            (make-instance 'project/repository/inspector :component-value project))
          (tab-page/widget (:selector (icon switch-to-tab-page :label "Test Suite"))
            (make-instance 'project/test/inspector :component-value project))
          (tab-page/widget (:selector (icon switch-to-tab-page :label "Licence"))
            (make-instance 'project/licence/inspector :component-value project)))))

;;;;;;
;;; project/description/inspector

(def (component e) project/description/inspector (inspector/style t/detail/presentation content/widget)
  ())

(def refresh-component project/description/inspector
  (bind (((:slots content component-value) -self-)
         (system (asdf:find-system (project-system-name component-value))))
    (setf content (or (when (slot-boundp system 'asdf::description)
                        (asdf:system-description system))
                      "No description"))))

;;;;;;
;;; project/content/inspector

(def (component e) project/content/inspector (inspector/style t/detail/presentation)
  ((directory :type component)))

(def refresh-component project/content/inspector
  (bind (((:slots directory component-value) -self-))
    (setf directory (make-value-inspector (path-of component-value) :initial-alternative-type 'pathname/directory/tree/inspector))))

(def render-xhtml project/content/inspector
  (with-render-style/abstract (-self-)
    (render-component (directory-of -self-))))

;;;;;;
;;; project/repository/inspector

(def (component e) project/repository/inspector (inspector/style t/detail/presentation)
  ())

(def render-xhtml project/repository/inspector
  (bind (((:slots component-value) -self-))
    (cond ((probe-file (merge-pathnames "_darcs" (path-of component-value)))
           ;; TODO: how do we get the repository entry point?
           <iframe (:width "100%" :height "600px" :style "border: none;"
                    :src `str("/darcsweb/darcsweb.cgi?r=LIVE " ,(name-of component-value) ";a=summary"))>)
          ((probe-file (merge-pathnames ".git" (path-of component-value)))
           ;; TODO: how do we get the repository entry point?
           <iframe (:width "100%" :height "600px" :style "border: none;"
                    :src `str("/gitweb/gitweb.cgi?p=" ,(name-of component-value) "/.git;a=summary"))>)
          (t <span "Unknown or no repository for " ,(name-of component-value)>))))

;;;;;;
;;; project/test/inspector

(def (component e) project/test/inspector (inspector/style t/detail/presentation content/widget)
  ())

(def refresh-component project/test/inspector
  (bind (((:slots content component-value) -self-)
         (test-package-name (system-package-name (asdf:find-system (system-test-system-name (asdf:find-system (project-system-name component-value)))))))
    (setf content (make-value-inspector (funcall (find-symbol "FIND-TEST" :hu.dwim.stefil) (find-symbol "TEST" test-package-name))
                                        :initial-alternative-type 'test/hierarchy/tree/inspector))))

;;;;;;
;;; project/licence/inspector

(def (component e) project/licence/inspector (inspector/style t/detail/presentation content/widget)
  ())

(def refresh-component project/licence/inspector
  (bind (((:slots content component-value) -self-)
         (licence-pathname (project-licence-pathname component-value)))
    (setf content (make-value-inspector licence-pathname :initial-alternative-type 'pathname/text-file/inspector))))
