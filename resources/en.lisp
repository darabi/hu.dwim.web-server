;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def resources en
  (action.cancel "cancel"))

(def resources en
  (mime-type.application/msword "Microsoft Word Document")
  (mime-type.application/vnd.ms-excel "Microsoft Excel Document")
  (mime-type.application/pdf "PDF Document")
  (mime-type.image/png "PNG image")
  (mime-type.image/tiff "TIFF image"))

;;; Error handling

(def function render-access-denied-error-page/english (&key &allow-other-keys)
  <div
   <h1 "Access denied">
   <p "You have no permission to access the requested resource.">
   <p <a (:href `js-inline(history.go -1)) "Go back">>>)

(def resources en
  (error.internal-server-error "Internal server error")
  (error.access-denied-error "Access denied")

  (render-internal-error-page (&rest args &key &allow-other-keys)
    (apply 'render-internal-error-page/english args))

  (render-access-denied-error-page (&rest args &key &allow-other-keys)
    (apply 'render-access-denied-error-page/english args)))
