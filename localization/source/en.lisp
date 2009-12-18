;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def localization en
  (class-name.symbol "symbol")
  (class-name.number "number")
  (class-name.integer "integer")
  (class-name.string "string")
  (class-name.system "system")
  (class-name.package "package")
  (class-name.standard-class "standard class")

  (class-name.definition "definition")
  (class-name.constant-definition "constant definition")
  (class-name.variable-definition "variable definition")
  (class-name.macro-definition "macro definition")
  (class-name.function-definition "function definition")
  (class-name.generic-function-definition "generic function definition")
  (class-name.type-definition "type definition")
  (class-name.structure-definition "structure definition")
  (class-name.condition-definition "condition definition")
  (class-name.class-definition "class definition")
  (class-name.package-definition "package definition")

  (slot-name.name "name")
  (slot-name.source-file "source file"))
