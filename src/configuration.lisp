;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(def macro debug-only (&body body)
  (if *load-as-production-p*
      (values)
      `(progn
         ,@body)))

(def macro debug-only* (&body body)
  `(unless *load-as-production-p*
     ,@body))

(def macro production-only (&body body)
  (if *load-as-production-p*
      `(progn
         ,@body)
      (values)))

(def macro production-only* (&body body)
  `(if *load-as-production-p*
       (progn
         ,@body)
       (values)))

(defun transform-function-definer-options (options)
  (if *load-as-production-p*
      options
      (remove-from-plist options :inline :optimize)))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-readtime-wrapper-syntax)
  (enable-quasi-quoted-xml-syntax :transformation *quasi-quoted-xml-transformation*)
  (enable-sharpquote<>-syntax))

(register-readtable-for-swank
 '("HU.DWIM.WUI" "HU.DWIM.WUI-USER" "HU.DWIM.WUI-TEST") 'setup-readtable)
