;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;; Error handling
(def resources hu
  (render-failed-to-load-page (&key &allow-other-keys)
    <div (:id ,+page-failed-to-load-id+)
     <h1 "Gyanúsan sokáig tart az oldal betöltése...">
     <p "Sajnos egyes böngészők néha összezavarodnak az oldal betöltése közben. Próbálja meg "
        <a (:href "#" :onclick "return _wui_handleFailedToLoad()") "újratölteni az oldalt">
        ", és ha az többszöri próbálkozásra sem segít, akkor törölje a böngésző gyorsítótárát.">>))

;;; Context sensitive help
(def resources hu
  (icon-label.help "Segítség")
  (help.no-context-sensitive-help-available "Nincs környezetfüggő segítség")
  (help.help-about-context-sensitive-help-button "Ez a környezetfüggő segítség üzemmódnak a ki- és bekapcsoló gombja. Segítség üzemmódban az egérrel megállva a képernyő különböző pontjain feljön egy hasonló buborék mint ez, ami megmutatja az adott pontra legrelevánsabb környezetfüggő segítséget (jelen esetben magának a segítség üzemmódnak a leírását). A segítség üzemmódot a kérdőjel formájú egér kurzor jelzi. Ilyenkor az egérrel bárhova kattintva a segítség üzemmód kikapcsol."))

;;; MetaGUI
(def resources hu
  (selectable-standard-object-tree-table-inspector.title (class)
    `xml,"Egy " (render class) `xml," kiválasztása")
  (standard-object-detail-filter.title (class)
    (render class) `xml," keresése")
  (standard-object-detail-filter.class-selector-label "Típus")
  (standard-object-detail-filter.ordering-specifier-label "Rendezés")
  (standard-object-list-table-inspector.title (class)
    `xml,"Egy " (render class) `xml," lista megjelenítése")
  (standard-object-tree-table-inspector.title (class)
    `xml,"Egy " (render class) `xml," fa megjelenítése")
  (standard-object-detail-inspector.title (class)
    `xml,"Egy " (render class) `xml," megjelenítése")

  (object-list-table.column.commands "")
  (object-list-table.column.type "Típus")

  (object-tree-table.column.commands "")
  (object-tree-table.column.type "Típus")
  )
