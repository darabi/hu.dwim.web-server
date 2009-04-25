;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object manager

(def component standard-object-manager (abstract-standard-class-component tab-container-component)
  ())

(def (macro e) standard-object-manager (class &body pages)
  `(make-instance 'standard-object-manager :the-class (find-class ,class) :pages (list ,@pages)))

(def method refresh-component :before ((self standard-object-manager))
     (bind (((:slots the-class pages) self))
       (setf pages (list* (tab-page (icon swith-to-page :label "Keresés")
                            (make-filter the-class))
                          (tab-page (icon swith-to-page :label "Létrehozás")
                            (make-maker the-class))
                          pages))))
