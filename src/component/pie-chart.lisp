;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object list pivot table

(def component pie-chart-component ()
  ((title :type string)
   (size :type list)
   (data :type list)
   (labels :type list)))

(def render pie-chart-component ()
  (bind (((:read-only-slots size data labels) -self-))
    <span ,(princ-to-string data)>
    <img (:src ,(concatenate-string "http://chart.apis.google.com/chart?cht=p3&chf=bg,s,00000000"
                                    (format nil "&chd=t:~{~A~^,~}" data)
                                    (format nil "&chs=~Ax~A" (first size) (second size))
                                    (format nil "&chl=~{~A~^|~}" labels)))>))

#|
;;;;;;
;;; Pie chart sheet

;; TODO: maybe it should be a table instead?
(def component pie-chart-sheet-component ()
  ((pie-charts nil :type components)))

(def render pie-chart-sheet-component ()
  (bind (((:read-only-slots pie-charts) -self-))
    <div
     ,@(mapcar #'render pie-charts)>))

;;;;;;
;;; Standard object list pie chart sheet

(def component standard-object-list-pie-chart-sheet-component (abstract-standard-object-list-component pie-chart-sheet-component)
  ())

(def function xxx ()
  (flet ((make-path-label (path)
           (apply #'concatenate 'string
                  (mapcar (lambda (category)
                            (component-value-of (content-of category)))
                          path))))
    (bind ((disc-axes nil)
           (pie-axes nil)
           (pie-charts nil))
      (apply #'map-product*
             (lambda (disc-path)
               (bind ((data nil)
                      (labels nil))
                 (apply #'map-product*
                        (lambda (pie-path)
                          (push (funcall aggregator ...) ;; TODO:
                                data)
                          (push (make-path-label pie-path) labels))
                        pie-axes)
                 (push (make-instance 'pie-chart
                                      :title (make-path-label disc-path)
                                      :size '(200 200)
                                      :data data
                                      :labels labels)
                       pie-charts)))
             disc-axes)
      (set (pie-charts-of self) pie-charts))))
|#