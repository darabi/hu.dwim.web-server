;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; context-sensitive-help

(def (constant :test #'string=) +context-sensitive-help-parameter-name+ "_hlp")

(def (icon e) help :tooltip nil)

(def (component e) context-sensitive-help (content/mixin id/mixin)
  ()
  (:default-initargs :content (icon help)))

(def render-xhtml context-sensitive-help
  (when *frame*
    (bind ((href (register-action/href (make-action (show-context-sensitive-help -self-)) :delayed-content #t)))
      <div (:id ,(id-of -self-)
            :onclick `js-inline(wui.help.setup event ,href)
            :onmouseover `js-inline(bind ((kludge (wui.help.make-mouseover-handler ,href)))
                                     ;; KLUDGE for now cl-qq-js chokes on ((wui.help.make-mouseover-handler ,href))
                                     (kludge event)))
           ,(call-next-method)>)))

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
        (make-component-rendering-response (make-instance 'context-sensitive-help-popup
                                                          :content (or (and components
                                                                            (make-context-sensitive-help (first components)))
                                                                       #"help.no-context-sensitive-help-available")))))))

(def (generic e) make-context-sensitive-help (component)
  (:method ((component component))
    nil)

  (:method :around ((component id/mixin))
    (or (call-next-method)
        (awhen (parent-component-of component)
          (make-context-sensitive-help it))))

  (:method ((self context-sensitive-help))
    #"help.help-about-context-sensitive-help-button"))

;;;;;;
;;; Context sensitive help popup

(def (component e) context-sensitive-help-popup (content/mixin)
  ())

(def render-xhtml context-sensitive-help-popup
  <div (:class "context-sensitive-help-popup")
       ,(call-next-method)>)

;;;;;;
;;; usage-help/widget

(def (component e) usage-help/widget (component-messages/widget title/mixin)
  ()
  (:default-initargs :title "Segítség"))

(def render-xhtml usage-help/widget
  (when (parameter-value +no-javascript-error-parameter-name+)
    (add-component-error-message -self- "Nincs engedélyezve az internet böngészőjében a JavaScript programok futtatása, így az alkalmazás sajnos nem használható. Kérjük engedélyezze a JavaScript futtatását a beállításokban!"))
  (unless (supported? (determine-user-agent *request*))
    (add-component-error-message -self- "Ezt az internet böngészőt vagy annak az éppen használt verzióját az alkalmazás nem támogatja. Az alábbi oldalon olvashatja a támogatott böngészők listáját és a letöltésükhöz szükséges információkat. A kellemetlenségért elnézését kérjük!"))
  <div (:class "usage-help widget")
    ,(render-title-for -self-)
    ,(render-component-messages-for -self-)
    <div (:class "timezone-info")
      ;; TODO get the timezone from local-time:*default-timezone*
      <p "Az oldalon megjelenített időpontok a magyar időzónában vannak.">>

    <div (:id "help")
      <h2 "Követelmények">

      <p "Az alkalmazás használatához szükséges technikai
          feltételek a következő pontokban olvashatók. Amennyiben
          az alább felsorolt szoftverek valamelyike nincs telepítve,
          kérjük forduljon a helyi rendszergazdájához segítségért.
          Bizonyos szoftverek telepítése a lap alján mellékelt linkek
          segítségével önállóan is elvégezhető.">
      <ul <h3 "Operációs rendszer">
          <p "Bármilyen operációs rendszer használható, amely
              rendelkezik megfelelő internetböngészővel. Többek között a
              Microsoft Windows 2000/XP és a Linux különféle verziói.">

          <h3 "Böngésző">
          <p "A " <b "Firefox 3+"> " internetböngésző használata javasolt! Ezen kívül 
              alkalmazható a " <b "Microsoft Internet Explorer 7+"> " és az " <b "Opera 9.6+"> " is.">

          <h3 "Képernyő felbontás">
          <p "Az ajánlott minimális képernyő felbontás 1024 x 768 pixel, az
              optimális képernyő felbontás pedig 1280 x 1024 pixel. A jobb
              olvashatóság érdekében ajánlott a böngészőt teljes képernyő
              (full screen) üzemmódban használni. Ez a funkció például a
              Firefox böngészőben az F11 gombbal ki- és bekapcsolható.">

          <h3 "JavaScript">
          <p "A böngészőben engedélyezni kell a JavaScript
              használatát. Amennyiben ez a funkció nincs engedélyezve, akkor
              az alkalmazás a kezdőlapon felhívja a figyelmet erre. Ebben az
              esetben a belépés nem lehetséges.">

          <h3 "Nyomtatás">
          <p "A dokumentumok nyomtatásához az Adobe Acrobat Reader program szükséges.">>>

      <h2 "Letöltések">
      <ul <h3 "Firefox">
          <ul <li <a (:href "http://www.mozilla-europe.org/hu/products/firefox/" :target "_blank") "magyar nyelvű">>
              <li <a (:href "http://www.mozilla-europe.org/en/products/firefox/" :target "_blank") "angol nyelvű">>>

          <h3 "Microsoft Internet Explorer">
          <ul <li <a (:href "https://www.microsoft.com/hun/windows/ie/downloads/default.mspx" :target "_blank") "magyar nyelvű">>
              <li <a (:href "https://www.microsoft.com/windows/ie/downloads/default.mspx" :target "_blank") "angol nyelvű">>>

          <h3 "Adobe Acrobat Reader">
          <ul <li <a (:href "http://letoltes.prim.hu/letoltes/program/58499/" :target "_blank") "magyar nyelvű">>
              <li <a (:href "http://www.adobe.com/products/acrobat/readstep2.html" :target "_blank") "angol nyelvű">>>>>)
