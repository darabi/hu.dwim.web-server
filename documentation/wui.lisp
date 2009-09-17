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
                  (hu.dwim.wui.test::make-demo-content)))))

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
