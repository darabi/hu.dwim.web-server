;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Table

(def component table-component ()
  ((columns nil :type components)
   (rows nil :type components)
   (page-navigation-bar nil :type component)))

(def constructor table-component ()
  (with-slots (page-navigation-bar) -self-
    (setf page-navigation-bar (make-instance 'page-navigation-bar-component :position 0 :page-count 10))))

(def render table-component ()
  (with-slots (columns rows page-navigation-bar) -self-
    (setf (total-count-of page-navigation-bar) (length rows))
    <div
      <table
        <thead
          <tr ,@(mapcar #'render columns)>>
      <tbody ,@(mapcar #'render
                       (subseq rows
                               (position-of page-navigation-bar)
                               (min (length rows)
                                    (+ (position-of page-navigation-bar)
                                       (page-count-of page-navigation-bar)))))>>
      ,(if (< (page-count-of page-navigation-bar) (total-count-of page-navigation-bar))
           (render page-navigation-bar)
           +void+)>))

(def component column-component (content-component)
  ())

(def render column-component ()
  <th ,(call-next-method)>)

(def component row-component ()
  ((cells nil :type components)))

(def render row-component ()
  <tr ,@(mapcar #'render (cells-of -self-))>)

(def component entire-row-component (content-component)
  ())

(def render entire-row-component ()
  <tr
    <td (:colspan ,(length (columns-of (parent-component-of -self-))))
      ,(call-next-method)>>)

(def component cell-component (content-component)
  ())

(def render cell-component ()
  <td ,(call-next-method)>)
