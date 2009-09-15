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
         (delay-alternative-component-with-initargs 'project/content/inspector :component-value value)
         (call-next-method)))

(def method localized-instance-name ((project project))
  (name-of project))

;;;;;;
;;; project/detail/inspector

(def (component e) project/detail/inspector (inspector/style t/detail/presentation tab-container/widget)
  ())

(def layered-method refresh-component :before ((self project/detail/inspector))
  (bind (((:slots hu.dwim.wui::tab-pages component-value) self))
    (setf hu.dwim.wui::tab-pages
          (list (tab-page/widget (:selector "Description")
                  "TODO")
                (tab-page/widget (:selector "Content")
                  (make-instance 'project/content/inspector :component-value component-value))
                (tab-page/widget (:selector "Repository")
                  (make-instance 'project/repository/inspector :component-value component-value))))))

;;;;;;
;;; project/content/inspector

(def (component e) project/content/inspector (inspector/style t/detail/presentation)
  ((directory :type component)))

(def refresh-component project/content/inspector
  (bind (((:slots directory) -self-)
         (component-value (component-value-of -self-)))
    (setf directory (make-value-inspector (path-of component-value) :initial-alternative-type 'pathname/directory/tree/inspector))))

(def render-xhtml project/content/inspector
  (with-render-style/abstract (-self-)
    (render-component (directory-of -self-))))

;;;;;;
;;; project/repository/inspector

(def (component e) project/repository/inspector (inspector/style t/detail/presentation)
  ())

(def render-xhtml project/repository/inspector
  ;; TODO:
  (bind ((component-value (component-value-of -self-)))
    (if (probe-file (merge-pathnames "_darcs" (path-of component-value)))
        <iframe (:width "100%" :height "600px" :style "border: none;"
                 :src `str("cgi-bin/darcsweb.cgi?r=LIVE " ,(name-of component-value) ";a=summary"))>
        <span "Unknown or no repository for " ,(name-of component-value)>)))
