;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; google-analytics/widget

(def (component e) google-analytics/widget ()
  ((analytics-account :type string)))

(def (macro e) google-analytics/widget (&rest args &key &allow-other-keys)
  `(make-instance 'google-analytics/widget ,@args))

(def render-xhtml google-analytics/widget
  (bind (((:read-only-slots analytics-account) -self-))
    `js(let ((gaJsHost (if (== "https:" document.location.protocol)
                           "https://ssl."
                           "http://www.")))
         (document.write (unescape (+ "%3Cscript src='" gaJsHost "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"))))
    `js(try (let ((pageTracker (_gat._getTracker ,analytics-account)))
              (pageTracker._trackPageview))
         (catch (e)
           nil))))
