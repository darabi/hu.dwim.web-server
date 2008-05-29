;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;; TODO: a lot is missing

;;;;;;
;;; Tree

(def component tree-component ()
  ((columns :type components)
   (root :type component)))

(def render tree-component ()
  (with-slots (columns root) self
    <table
      <thead
        <tr ,@(mapcar #'render columns)>>
      <tbody ,(render root)>>))

(def component node-component ()
  ((children :type components)
   (cells :type components)))

(def render node-component ()
  (with-slots (children cells) self
    (append (list <tr ,(mapcar #'render cells)>)
            (mapcar #'render children))))
