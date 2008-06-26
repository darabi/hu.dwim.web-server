;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object list pivot table

(def component pie-chart-component ()
  ((size)
   (data)
   (labels)))

(def render pie-chart-component ()
  (bind (((:read-only-slots size data labels) -self-))
    <img (:src ,(concatenate-string "http://chart.apis.google.com/chart?cht=p3&chf=bg,s,00000000"
                                    (format nil "&chd=t:~{~A~^,~}" data)
                                    (format nil "&chs=~Ax~A" (first size) (second size))
                                    (format nil "&chl=~{~A~^|~}" labels)))>))
