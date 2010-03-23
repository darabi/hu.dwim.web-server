;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def localization hu
  (class-name.symbol "symbol")
  (class-name.number "szám")
  (class-name.integer "egész")
  (class-name.string "szöveg")
  (class-name.system "system")
  (class-name.package "package")
  (class-name.standard-class "standard osztály")

  (class-name.definition "definíció")
  (class-name.constant-definition "állandó definíció")
  (class-name.variable-definition "változó definíció")
  (class-name.macro-definition "macro definíció")
  (class-name.function-definition "funkció definíció")
  (class-name.generic-function-definition "generic funkció definíció")
  (class-name.type-definition "típus definíció")
  (class-name.structure-definition "struktúra definíció")
  (class-name.condition-definition "kivétel definíció")
  (class-name.class-definition "osztály definíció")
  (class-name.package-definition "package definíció")

  (slot-name.name "név")
  (slot-name.%type "típus")
  (slot-name.source-file "forrás fájl"))
