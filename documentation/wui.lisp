;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui.documentation)

(def project :hu.dwim.wui :path (hu.dwim.asdf:system-pathname :hu.dwim.wui))

(def method make-project-tab-pages ((component project/detail/inspector) (project (eql (find-project :hu.dwim.wui))))
  (append (call-next-method)
          (list (tab-page/widget (:selector "Demo")
                  (hu.dwim.wui.test::make-demo-content))
                (tab-page/widget (:selector "Documentation")
                  (make-value-inspector (find-book 'user-guide) :initial-alternative-type 'book/text/inspector)))))

(def book user-guide (:title "hu.dwim.wui")
  (chapter (:title "Introduction")
    (chapter (:title "What is hu.dwim.wui?")
      )
    (chapter (:title "Why not something else?")
      ))
  (chapter (:title "Supported Platforms")
    (chapter (:title "Operating Systems")
      )
    (chapter (:title "Common Lisp Implementations")
      ))
  (chapter (:title "Instal Guide")
    )
  (chapter (:title "Start Server")
    )
  (chapter (:title "Tutorial")
    (chapter (:title "Hello world")
      ))
  (chapter (:title "Concepts")
    (chapter (:title "HTTP Server")
      (chapter (:title "Server")
        )
      (chapter (:title "Server Listen Entry")
        )
      (chapter (:title "Entry point")
        ))
    (chapter (:title "Application Server")
      (chapter (:title "Application")
        )
      (chapter (:title "Session")
        )
      (chapter (:title "Frame")
        )
      (chapter (:title "Request")
        )
      (chapter (:title "Response")
        )
      (chapter (:title "Action")
        )
      (chapter (:title "File Serving")
        )
      (chapter (:title "JavaScript File Serving")
        ))
    (chapter (:title "Component Server")
      (chapter (:title "Component")
        )
      (chapter (:title "Immediate")
        )
      (chapter (:title "Layout")
        )
      (chapter (:title "Widget")
        )
      (chapter (:title "Presentation")
        (chapter (:title "Maker")
          )
        (chapter (:title "Viewer")
          )
        (chapter (:title "Editor")
          )
        (chapter (:title "Inspector")
          )
        (chapter (:title "Filter")
          ))
      (chapter (:title "Text")
        (chapter (:title "Book")
          )
        (chapter (:title "Chapter")
          )
        (chapter (:title "Paragraph")
          ))
      (chapter (:title "Source")
        (chapter (:title "Variable")
          )
        (chapter (:title "Function")
          )
        (chapter (:title "Class")
          )
        (chapter (:title "Dictionary")
          )
        (chapter (:title "Package")
          )
        (chapter (:title "System")
          ))
      (chapter (:title "Chart")
        (chapter (:title "Flow")
          ))
      (chapter (:title "Authorization")
        (chapter (:title "Login")
          )
        (chapter (:title "Logout")
          )
        (chapter (:title "Access Control")
          )))))

#|
1. WUI
------

An all-lisp web server, based on iolib.

(test-system :hu.dwim.wui)

(start-test-server-with-wudemo-application)

http://localhost.localdomain:8080/

(setf hu.dwim.wui::*debug-on-error* t)

2. DOJO
-------

You need to build dojo for WUI to work:

svn checkout -r 18738 http://svn.dojotoolkit.org/src/trunk/ dojo/
svn update -r 18738
~/workspace/wui/etc/build-dojo.sh --dojo ~/workspace/dojo --dojo-release-dir ~/workspace/wui/wwwroot/ --profile ~/workspace/wui/etc/wui.profile.js --locales "en-us,hu"
|#
