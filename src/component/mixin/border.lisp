;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Border mixin

(def (component e) border/mixin ()
  ((border :type component))
  (:documentation "A COMPONENT with a BORDER."))

(def (function e) render-with-border (style-class thunk)
  <table (:class ,style-class :style "clear: both;")
    <thead <tr (:class "border-top")
               <td (:class "border-left")>
               <td (:class "border-center")>
               <td (:class "border-right")>>>
    <tbody <tr (:class "border-center")
               <td (:class "border-left")>
               <td (:class "border-center")
                   ,(funcall thunk)>
               <td (:class "border-right")>>>
    <tfoot <tr (:class "border-bottom")
               <td (:class "border-left")>
               <td (:class "border-center")>
               <td (:class "border-right")>>>>)
