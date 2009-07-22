;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def resources hu
  (render-frame-out-of-sync-error (refresh-href new-frame-href &key &allow-other-keys)
    <div
     <p "A böngésző ablak nincs szinkronban a szerverrel...">
     <p "Ez egy komplex alkalmazás, ami nem teljesen úgy működik mint egy szokásos weboldal, ezért kérjük ne használja a böngésző " <i>"Vissza"</i> " gombját és a " <i>"Megnyitás új ablakban"</i> " parancsot, valamint ne másoljon ki hivatkozásokat sem a böngészőből! Kérjük, hogy az alkalmazás gombjaiat használja ezen műveletek elvégzéséhez!">
     <p <a (:href ,refresh-href) "Vissza az alkalmazáshoz">>
     <p <a (:href ,new-frame-href) "Új nézet az alkalmazásra">>>)
  (render-application-internal-error-page (&key administrator-email-address &allow-other-keys)
    <div
     <p "A hibáról értesülni fognak a fejlesztők és remélhetőleg a közeljövőben javítják azt.">
     ,(when administrator-email-address
        <p "Amennyiben kapcsolatba szeretne lépni az üzemeltetőkkel, azt a "
           <a (:href ,(mailto-href administrator-email-address)) ,administrator-email-address>
           " email címen megteheti.">)>)
  (error.internal-server-error.title "Programhiba")
  (error.internal-server-error.message "Váratlan hiba történt az előző művelet végrehajtása közben. Elnézést kérünk az esetleges kellemetlenségért!")
  )
