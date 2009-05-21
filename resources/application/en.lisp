;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def resources en
  (render-frame-out-of-sync-error (refresh-href new-frame-href &key &allow-other-keys)
    <div
      <p "Browser window went out of sync with the server...">
      <p "Please avoid using the " <i "Back">
         " button of your browser and/or opening links in new windows by copy-pasting URL's or using the " <i "Open in new window">
         " feature of your browser. To achieve the same effect, you can use the navigation actions provided by the application.">
      <p <a (:href ,refresh-href) "Bring me back to the application">>
      <p <a (:href ,new-frame-href) "Reset my view of the application">>>)
  (render-application-internal-error-page (&key admin-email-address &allow-other-keys)
    <div
      <p "The developers will be notified about this error and they will hopefully fix it soon.">
      ,(when admin-email-address
         <p "You may contact the administrators at this email address: "
             <a (:href ,(mailto-href admin-email-address)) ,admin-email-address>>)>)
  (error.internal-server-error.title "Internal server error")
  (error.internal-server-error.message "An internal server error has occured, we are sorry for the inconvenience.")
  )
