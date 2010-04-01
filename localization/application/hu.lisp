;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def localization hu
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
  )

(def js-localization hu
  (error.ajax.request-to-invalid-session "Megszakadt a kapcsolat a szerverrel. Az oldal újratöltésével újból felépítheti a kapcsolatot...")
  (error.network-error.title "Kommunikációs hiba")
  (error.network-error "Hiba történt a szerverrel való kommunikáció közben. Ezt okozhatja átmeneti hálózati hiba, ezért kérem próbálkozzon újra, és/vagy próbálja meg újratölteni az oldalt! Amennyiben a hiba huzamosan fennáll, akkor kérem lépjen kapcsolatba az üzemeltetőkkel!")
  (error.generic-javascript-error.title "Váratlan kliens oldali hiba")
  (error.generic-javascript-error "Hiba történt a böngészőben a program futtatása közben. Ezt okozhatja a böngésző sajátos működése is, ezért kérem frissítse a böngészőjét, és/vagy próbálja meg másik böngészővel! Amennyiben a hiba huzamosan fennáll egy támogatott böngészővel is, akkor kérem lépjen kapcsolatba az üzemeltetőkkel!")
  (error.internal-server-error.title "Programhiba")
  (error.internal-server-error "Váratlan hiba történt az előző művelet végrehajtása közben. Elnézést kérünk az esetleges kellemetlenségért!")
  (action.reload-page "Az oldal újratóltése")
  (action.cancel "Mégse")
  )
