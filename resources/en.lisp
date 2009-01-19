;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def resources en
  (mime-type.application/msword "Microsoft Word Document")
  (mime-type.application/vnd.ms-excel "Microsoft Excel Document")
  (mime-type.application/pdf "PDF Document")
  (mime-type.image/png "PNG image")
  (mime-type.image/tiff "TIFF image"))

;;; Error handling
(def resources en
  (error.internal-server-error "Internal server error")
  (render-internal-error-page (&rest args &key &allow-other-keys)
    (apply 'render-internal-error-page/english args))

  (error.access-denied-error "Access denied")
  (render-access-denied-error-page (&rest args &key &allow-other-keys)
    (apply 'render-access-denied-error-page/english args)))
