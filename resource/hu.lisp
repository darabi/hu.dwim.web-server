;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Mime type

(def resources hu
  (mime-type.application/msword "Microsoft Word Dokumentum")
  (mime-type.application/vnd.ms-excel "Microsoft Excel Dokumentum")
  (mime-type.application/pdf "PDF Dokumentum")
  (mime-type.image/png "PNG kép")
  (mime-type.image/tiff "TIFF kép"))

;;;;;;
;;; Error handling

(def resources hu
  (error.internal-server-error "Ismeretlen eredetű hiba")
  (error.access-denied-error "Hozzáférés megtagadva")

  (render-internal-error-page (&key admin-email-address &allow-other-keys)
    <div
      <h1 "Programhiba">
      <p "A szerverhez érkezett kérés feldolgozása közben váratlan hiba történt. Elnézést kérünk az esetleges kellemetlenségért!">
      <p "A hibáról értesülni fognak a fejlesztők és valószínűleg a közeljövőben javítják azt.">
      ,(when admin-email-address
         <p "Amennyiben kapcsolatba szeretne lépni az üzemeltetőkkel, azt a "
            <a (:href ,(mailto-href admin-email-address)) ,admin-email-address>
            " email címen megteheti.">)
      <p <a (:href "#" :onClick `js-inline(history.go -1)) "Vissza">>>)

  (render-access-denied-error-page (&key &allow-other-keys)
    <div
      <h1 "Hozzáférés megtagadva">
      <p "Nincs joga a kívánt oldal megtekintéséhez.">
      <p <a (:href "#" :onClick `js-inline(history.go -1)) "Vissza">>>))