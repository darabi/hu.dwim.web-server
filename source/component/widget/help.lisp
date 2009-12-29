;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; context-sensitive-help

(def constant +context-sensitive-help-parameter-name+ "_hlp")

(def (icon e) context-sensitive-help :tooltip nil)

(def (component e) context-sensitive-help (content/mixin frame-unique-id/mixin)
  ()
  (:default-initargs :content (icon context-sensitive-help)))

(def render-xhtml context-sensitive-help
  (when *frame*
    (bind ((href (register-action/href (make-action (show-context-sensitive-help -self-)) :delayed-content #t)))
      <div (:id ,(id-of -self-)
            :onclick `js-inline(wui.help.setup event ,href)
            :onmouseover `js-inline((wui.help.make-mouseover-handler ,href) event))
        ,(render-content-for -self-)>)))

(def layered-function show-context-sensitive-help (component)
  (:method ((self context-sensitive-help))
    (with-request-params (((ids +context-sensitive-help-parameter-name+) nil))
      (setf ids (ensure-list ids))
      (bind ((components nil))
        (map-descendant-components (root-component-of *frame*)
                                   (lambda (descendant)
                                     (when (and (typep descendant 'id/mixin)
                                                (member (id-of descendant) ids :test #'string=))
                                       (push descendant components))))
        (make-component-rendering-response (or (some (lambda (component)
                                                       (make-context-sensitive-help component (component-dispatch-class component) (component-dispatch-prototype component) (component-value-of component)))
                                                     components)
                                               #"context-sensitive-help.not-available"))))))

(def layered-method make-context-sensitive-help ((component context-sensitive-help) class prototype value)
  #"context-sensitive-help.self-description")

;;;;;;
;;; usage-help/widget

(def (component e) usage-help/widget (component-messages/widget remote-setup/mixin title/mixin)
  ()
  (:default-initargs :title (title/widget () "Segítség")))

(def render-xhtml usage-help/widget
  (when (parameter-value +no-javascript-error-parameter-name+)
    (add-component-error-message -self- "*** Nincs engedélyezve az internet böngészőjében a JavaScript programok futtatása, így az alkalmazás sajnos egyátalán nem használható. Kérjük engedélyezze a JavaScript futtatását a beállításokban!"))
  (unless (supported? (identify-http-user-agent *request*))
    (add-component-error-message -self- "Ezt az internet böngészőt vagy annak az éppen használt verzióját az alkalmazás nem támogatja. Az alábbi oldalon olvashatja a támogatott böngészők listáját és a letöltésükhöz szükséges információkat. A kellemetlenségért elnézését kérjük!"))
  <div (:id ,(id-of -self-) :class "usage-help widget")
    ,(render-title-for -self-)
    ,(render-component-messages-for -self-)
    ;; TODO: make this a book and localize it
    <div (:id "help")
      <h2 "Követelmények">
      <p "Az alkalmazás használatához szükséges technikai feltételek a következő pontokban olvashatók. Amennyiben
          az alább felsorolt szoftverek valamelyike nincs telepítve, kérjük forduljon a helyi rendszergazdájához segítségért.
          Bizonyos szoftverek telepítése a lap alján mellékelt linkek segítségével önállóan is elvégezhető.">
      <ul <h3 "Operációs rendszer">
          <p "Bármilyen operációs rendszer használható, amely rendelkezik megfelelő internet böngészővel. Tesztelt operációs rendszerek:">
          <ul <li "Linux">
              <li "Microsoft Windows">
              <li "Mac OS X">>
          <h3 "Támogatott böngészők">
          <p "Bármilyen internet böngésző használható, amely képes JavaScript futtatására és támogatja az XHTML, SVG, CSS szabványokat. Tesztelt böngészők:">
          <ul <li "Google Chrome 4+">
              <li "Firefox 3+">
              <li "Opera 9.6+">
              <li "Internet Explorer 7+">
              <li "Safari 4+">
              <li "Konqueror 4.2+">>
          <h3 "Képernyő felbontás">
          <p "Az ajánlott minimális képernyő felbontás 1024 x 768 pixel, az optimális képernyő felbontás pedig 1280 x 1024 pixel. A jobb
              olvashatóság érdekében ajánlott a böngészőt teljes képernyő (full screen) üzemmódban használni. Ez a funkció a legtöbb
              böngészőben az F11 gombbal ki- és bekapcsolható.">
          <h3 "JavaScript">
          <p "A böngészőben engedélyezni kell a JavaScript használatát. Amennyiben ez a funkció nincs engedélyezve, akkor
              az alkalmazás a kezdőlapon felhívja a figyelmet erre. Ebben az esetben a belépés és a használat egyátalán nem lehetséges.">
          <h3 "Időzóna">
          ;; TODO get the timezone from local-time:*default-timezone*
          <p "Az oldalon megjelenített időpontok a magyar időzónában vannak.">
          <h3 "Nyomtatás">
          <p "A dokumentumok nyomtatásához az Adobe Acrobat Reader program szükséges.">>>
      <h2 "Letöltések">
      <ul <h3 "Google Chrome">
          <ul <li <a (:href "http://www.google.com/chrome" :target "_blank") "magyar nyelvű">>
              <li <a (:href "http://www.google.com/chrome" :target "_blank") "angol nyelvű">>>
          <h3 "Firefox">
          <ul <li <a (:href "http://www.mozilla-europe.org/hu/products/firefox/" :target "_blank") "magyar nyelvű">>
              <li <a (:href "http://www.mozilla-europe.org/en/products/firefox/" :target "_blank") "angol nyelvű">>>
          <h3 "Opera">
          <ul <li <a (:href "http://www.opera.com/" :target "_blank") "magyar nyelvű">>
              <li <a (:href "http://www.opera.com/" :target "_blank") "angol nyelvű">>>
          <h3 "Internet Explorer">
          <ul <li <a (:href "https://www.microsoft.com/hun/windows/ie/downloads/default.mspx" :target "_blank") "magyar nyelvű">>
              <li <a (:href "https://www.microsoft.com/windows/ie/downloads/default.mspx" :target "_blank") "angol nyelvű">>>
          <h3 "Adobe Acrobat Reader">
          <ul <li <a (:href "http://letoltes.prim.hu/letoltes/program/58499/" :target "_blank") "magyar nyelvű">>
              <li <a (:href "http://www.adobe.com/products/acrobat/readstep2.html" :target "_blank") "angol nyelvű">>>>>)
