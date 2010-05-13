;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; project/alternator/inspector

(def (component e) project/alternator/inspector (t/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null project) project/alternator/inspector)

(def layered-method make-alternatives ((component project/alternator/inspector) (class standard-class) (prototype project) (value project))
  (list* (make-instance 'project/detail/inspector :component-value value) (call-next-layered-method)))

(def method localized-instance-name ((project project))
  (name-of project))

;;;;;;
;;; project/detail/inspector

(def (component e) project/detail/inspector (t/detail/inspector tab-container/widget)
  ())

(def layered-method refresh-component :before ((self project/detail/inspector))
  (bind (((:slots hu.dwim.wui::tab-pages component-value) self))
    (setf hu.dwim.wui::tab-pages (make-project-tab-pages self component-value))))

(def (generic e) make-project-tab-pages (component project)
  (:method ((component project/detail/inspector) (project project))
    (list (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Description"))
            (make-instance 'project/description/inspector :component-value project))
          (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "System"))
            (make-instance 'project/system/inspector :component-value project))
          (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Content"))
            (make-instance 'project/content/inspector :component-value project))
          (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Repository"))
            (make-instance 'project/repository/inspector :component-value project))
          (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Test Suite"))
            (make-instance 'project/test/inspector :component-value project))
          (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Licence"))
            (make-instance 'project/licence/inspector :component-value project)))))

;;;;;;
;;; project/description/inspector

(def (component e) project/description/inspector (t/detail/inspector content/widget)
  ())

(def refresh-component project/description/inspector
  (bind (((:slots content component-value) -self-))
    (setf content (make-value-inspector (description-of component-value)))))

;;;;;;
;;; project/system/inspector

(def (component e) project/system/inspector (t/detail/inspector content/widget)
  ())

(def refresh-component project/system/inspector
  (bind (((:slots content component-value) -self-))
    (setf content (make-value-inspector (asdf:find-system (project-system-name component-value) #f)))))

;;;;;;
;;; project/content/inspector

(def (component e) project/content/inspector (t/detail/inspector)
  ((directory :type component)))

(def refresh-component project/content/inspector
  (bind (((:slots directory component-value) -self-))
    (setf directory (make-value-inspector (path-of component-value) :initial-alternative-type 'pathname/directory/tree/inspector))))

(def render-xhtml project/content/inspector
  (with-render-style/component (-self-)
    (render-component (directory-of -self-))))

;;;;;;
;;; project/repository/inspector

(def (component e) project/repository/inspector (t/detail/inspector)
  ())

(def render-xhtml project/repository/inspector
  (bind (((:slots component-value) -self-)
         (project-name (string-downcase (name-of component-value))))
    (cond ((probe-file (merge-pathnames "_darcs" (path-of component-value)))
           ;; TODO: how do we get the repository entry point?
           <iframe (:width "100%" :height "600px" :style "border: none;"
                    :src `str("/darcsweb/darcsweb.cgi?r=LIVE " ,project-name ";a=summary"))>)
          ((probe-file (merge-pathnames ".git" (path-of component-value)))
           ;; TODO: how do we get the repository entry point?
           <iframe (:width "100%" :height "600px" :style "border: none;"
                    :src `str("/gitweb/gitweb.cgi?p=" ,project-name "/.git;a=summary"))>)
          (t <span "Unknown or no repository for " ,project-name>))))

;;;;;;
;;; project/test/inspector

(def (component e) project/test/inspector (t/detail/inspector content/widget)
  ())

(def refresh-component project/test/inspector
  (bind (((:slots content component-value) -self-)
         (project-system (asdf:find-system (project-system-name component-value) #f))
         (test-system (when (and project-system
                                 (typep project-system 'hu.dwim.system))
                        (asdf:find-system (system-test-system-name project-system) #f)))
         (test-package-name (when test-system
                              (system-package-name test-system)))
         (test-name (when test-package-name
                      (find-symbol "TEST" test-package-name))))
    (setf content (if test-name
                      (make-value-inspector (funcall (find-symbol "FIND-TEST" :hu.dwim.stefil)
                                                     test-name)
                                            :initial-alternative-type 'test/hierarchy/tree/inspector)
                      "No test suite was found for this project"))))

;;;;;;
;;; project/licence/inspector

(def (component e) project/licence/inspector (t/detail/inspector content/widget)
  ())

(def refresh-component project/licence/inspector
  (bind (((:slots content component-value) -self-)
         (licence-pathname (project-licence-pathname component-value)))
    (setf content (make-value-inspector licence-pathname :initial-alternative-type 'pathname/text-file/inspector))))
