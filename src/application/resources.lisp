;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def function render-frame-out-of-sync-error/english (refresh-href new-frame-href &key &allow-other-keys)
  <div
   <p "Browser window went out of sync with the server...">
   <p "Please avoid using the " <i>"Back"</i> " button of your browser and/or opening links in new windows by copy-pasting URL's or using the " <i>"Open in new window"</i> " feature of your browser. To achieve the same effect, you can use the navigation actions provided by the application.">
   <p <a (:href ,refresh-href) "Bring me back to the application">>
   <p <a (:href ,new-frame-href) "Reset my view of the application">>>)

(def resources en
  (render-frame-out-of-sync-error (refresh-href new-frame-href &key &allow-other-keys)
    (render-frame-out-of-sync-error/english refresh-href new-frame-href)))

(def resources hu
  (render-frame-out-of-sync-error (refresh-href new-frame-href &key &allow-other-keys)
    <div
     <p "A böngésző ablak nincs szinkronban a szerverrel...">
     <p "Ez egy komplex alkalmazás, ami nem teljesen úgy működik mint egy szokásos weboldal, ezért kérjük ne használja a böngésző " <i>"Vissza"</i> " gombját és a " <i>"Megnyitás új ablakban"</i> " parancsot, valamint ne másoljon ki hivatkozásokat sem a böngészőből! Kérjük, hogy az alkalmazás gombjaiat használja ezen műveletek elvégzéséhez!">
     <p <a (:href ,refresh-href) "Vissza az alkalmazáshoz">>
     <p <a (:href ,new-frame-href) "Új nézet az alkalmazásra">>>))
