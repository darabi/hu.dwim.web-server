;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; XY container layout

(def (component e) xy/layout (layout/minimal contents/abstract)
  ((width :type number)
   (height :type number))
  (:documentation "A LAYOUT that positions CHILD-COMPONENTs to their specified X, Y coordinates within its own coordinate system."))

(def (macro e) xy/layout ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'xy/layout ,@args :contents (list ,@contents)))

(def render-xhtml xy/layout
  (not-yet-implemented))

;;;;;;
;;; Position layout

(def (component e) parent-relative-position/layout (layout/minimal content/abstract)
  ((x :type number)
   (y :type number))
  (:documentation "A LAYOUT that positions its CONTENT to the specified X, Y coordinates within its parent's coordinate system."))

(def (macro e) parent-relative-position/layout ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'parent-relative-position/layout ,@args :content ,(the-only-element content)))

(def render-xhtml parent-relative-position/layout
  (not-yet-implemented))
