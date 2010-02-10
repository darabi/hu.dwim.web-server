;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;; KLUDGE, TODO, FIXME, etc...
;; - (in-package :hu.dwim.util)
;; - numerous exported common symbols
;; - it doesn't belong to wui, but it doesn't have a place yet

(def (class* e) curriculum-vitae ()
  ((subject :type person)
   (educations :type list)
   (experiences :type list)
   (natural-languages :type list)
   (computer-languages :type list)
   (skills :type string)
   (interests :type string)))

(def (class* e) person ()
  ((photo :type pathname)
   (first-name :type string)
   (last-name :type string)
   (birth-date :type string)
   (email-address :type string)
   (mobile-phone :type string)
   (skype-id :type string)
   (facebook-id :type string)))

(def (class* e) education ()
  ((school :type school)
   (begin-date :type string)
   (end-date :type string)))

(def (class* e) school ()
  ((name :type string)
   (address :type string)
   (home-page :type string)))

(def (class* e) experience ()
  ((company :type company)
   (begin-date :type string)
   (end-date :type string)))

(def (class* e) company ()
  ((name :type string)
   (address :type string)
   (home-page :type string)))

(def (class* e) natural-language ()
  ((name :type string)
   (level :type string)))

(def (class* e) computer-language ()
  ((name :type string)
   (level :type string)
   (years-of-experience :type number)))

(def (namespace e) curriculum-vitae (&rest args &key &allow-other-keys)
  `(make-instance 'curriculum-vitae ,@args))

(def curriculum-vitae levente-mészáros
    :subject (make-instance 'person
                            :first-name "Levente" :last-name "Mészáros" :birth-date "1975-05-08"
                            :email-address "levente.meszaros@gmail.com" :mobile-phone "+36205413889" :skype-id "123456" :facebook-id "123456"
                            :photo (system-relative-pathname :hu.dwim.wui "levente-mészáros"))
    :educations (list (make-instance 'education
                                     :begin-date "1989" :end-date "1993"
                                     :school (make-instance 'school
                                                            :name "Egressy Gábor Ipari Szakközépiskola"
                                                            :home-page "TODO"))
                      (make-instance 'education
                                     :begin-date "1993" :end-date "1999"
                                     :school (make-instance 'school
                                                            :name "Budapesti Műszaki Egyetem"
                                                            :home-page "http://www.bme.hu")))
    :experiences (list (make-instance 'experience
                                      :begin-date "1999" :end-date "2001"
                                      :company (make-instance 'company
                                                              :name "ODD Informatikai Kft."
                                                              :home-page "http://odd.hu/")))
    :natural-languages (list (make-instance 'natural-language
                                            :name "Hungarian"
                                            :level "Mother")
                             (make-instance 'natural-language
                                            :name "English"
                                            :level "Fluent")
                             (make-instance 'natural-language
                                            :name "German"
                                            :level "Basic"))
    :computer-languages (list (make-instance 'computer-language
                                             :name "Common Lisp"
                                             :level "Experienced")
                              (make-instance 'computer-language
                                             :name "Java"
                                             :level "Experienced")
                              (make-instance 'computer-language
                                             :name "C++"
                                             :level "Experienced")))

(in-package :hu.dwim.wui)

;;;;;;
;;; curriculum-vitae/detail/inspector

(def (component e) curriculum-vitae/detail/inspector (t/slot-value-contents/inspector t/detail/presentation)
  ((slot-names t)))

(def render-xhtml curriculum-vitae/detail/inspector
  <div <div ,(render-component (find-slot-value-component -self- 'hu.dwim.util::subject))>
       <div ,(render-component (find-slot-value-component -self- 'hu.dwim.util::educations))>
       <div ,(render-component (find-slot-value-component -self- 'hu.dwim.util::experiences))>>)

(def layered-method make-alternatives ((component t/inspector) class prototype (value curriculum-vitae))
  (list* (make-instance 'curriculum-vitae/detail/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; person/detail/inspector

(def (component e) person/detail/inspector (t/slot-value-contents/inspector t/detail/presentation)
  ((slot-names '(hu.dwim.util::first-name hu.dwim.util::last-name hu.dwim.util::email-address))))

(def render-xhtml person/detail/inspector
  (with-render-style/abstract (-self-)
    <span (:class "name")
          ,(render-component (find-slot-value-component -self- 'hu.dwim.util::first-name))
          " "
          ,(render-component (find-slot-value-component -self- 'hu.dwim.util::last-name))>
    <span (:class "email-address")
          ,(render-component (find-slot-value-component -self- 'hu.dwim.util::email-address))>))

(def layered-method make-alternatives ((component t/inspector) class prototype (value person))
  (list* (make-instance 'person/detail/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; t/slot-value-contents/inspector

(def (component e) t/slot-value-contents/inspector (inspector/style contents/widget)
  ((slot-names t :allocation :class)))

(def refresh-component t/slot-value-contents/inspector
  (bind (((:slots component-value contents) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (slots (collect-slot-value-list/slots -self- class prototype component-value)))
    (setf contents
          (iter (for slot :in slots)
                (for slot-value = nil #+nil (find)) ;; TODO:
                (for slot-value-place = (make-slot-value-place component-value slot))
                (if slot-value
                    (setf (component-value-of slot-value) slot-value-place)
                    (setf slot-value (make-value-inspector slot-value-place)))
                (collect slot-value)))))

(def layered-method collect-slot-value-list/slots ((component t/slot-value-contents/inspector) class prototype value)
  (bind ((slot-names (slot-names-of component))
         (slots (class-slots class)))
    (if (eq slot-names t)
        slots
        (filter-slots slot-names slots))))

(def function find-slot-value-component (component slot-name &key (otherwise '(:error "Cannot find component for slot")))
  (or (find slot-name (contents-of component) :key [slot-definition-name (slot-of (component-value-of !1))])
      (handle-otherwise otherwise)))
