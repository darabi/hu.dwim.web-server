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
(def resources en
  (error.internal-server-error "Internal server error")
  (error.access-denied-error "Access denied")

  (render-internal-error-page (&key admin-email-address &allow-other-keys)
    <div
      <h1 "Internal server error">
      <p "An internal server error has occured while processing your request. We are sorry for the inconvenience.">
      <p "The developers will be notified about this error and will hopefully fix it in the near future.">
      ,(when admin-email-address
         <p "You may contact the administrators at the "
            <a (:href ,(mailto-href admin-email-address)) ,admin-email-address>
            " email address.">)
      <p <a (:href "#" :onClick `js-inline(history.go -1)) "Go back">>>)

  (render-access-denied-error-page (&key &allow-other-keys)
    <div
      <h1 "Access denied">
      <p "You have no permission to access the requested resource.">
      <p <a (:href "#" :onClick `js-inline(history.go -1)) "Go back">>>))
