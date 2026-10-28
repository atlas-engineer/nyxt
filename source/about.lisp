;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defparameter *about-contributors*
  '(("Adom Hartell" "@4t0m")
    ("André A. Gomes" "@aadcg")
    ("Artyom Bologov" "@aartaka")
    ("John Mercouris" "@jmercouris")
    ("@hendursaga")
    ("@kssytsrk")
    ("Pedro Delfino" "@pdelfino")
    ("Pierre Neidhardt" "@ambrevar")
    ("Solomon Bloch" "@noogie13")
    ("Vincent Dardel" "@vindarel"))
  "Names and handles shown under Contributors.")

(defparameter *about-campaigns*
  '(("2018-11 campaign: *NIX Support"
     ("Digital Omnipresence"
      "Alexander.Shendi" "Ashish SHUKLA" "Christopher Nascone" "dan.girsh"
      "Eric Monson" "Jack Randall" "James Anderson" "liweitian" "Marco Heisig"
      "Oluwafemi Agbabiaka" "pjb" "Robert Krahn" "Robert Uhl" "1 anonymous")
     ("Digital Magma"
      "Daniel V" "Jason Hamilton" "Magnus Nyberg" "Marek Kochanowicz"
      "Rich Smith" "Robert Uhl" "simon" "slade" "Steve Last" "ulf.makestad"
      "1 anonymous")
     ("Digital Immortality"
      "Alexey Abramov" "Are Jensen" "Joseph Mingrone" "Nikita Poliakov" "pjb"
      "Sainath Adapa" "Spencer Heywood" "Sungjin Chun" "Tom Delord"
      "2 anonymous")
     ("Others"
      "Nicholas Zivkovic" "Pierre Neidhardt" "Simon Zugmeyer" "vindarel"
      "5 anonymous"))
    ("2019-10 campaign: v1.4.0"
     ("Digital Immortality"
      "Tim Johann" "Julien Rousé" "ebababi" "Emil Oppeln-Bronikowski"
      "Fox Kiester" "Stefan Husmann" "Nils Grunwald" "Florian Adamsky"
      "Valentin Atanasov" "Pranav Vats" "Jörn Gersdorf" "Matt Skinner"
      "Jelle Dirk Licht" "Minori Yamashita" "Hugh Daschbach" "Niklas Carlsson"
      "mestelan" "Camille Troillard" "mace nicolas" "dan.girsh"
      "Michael Bruderer" "Patrice Rault" "Cees de Groot" "Sam Hedin" "rbarzic"
      "Jake Waksbaum" "Lukas Jenks" "Rodrigo Lazo" "Lucas Sifoni"
      "Calle Helmertz" "Kristian Nygaard Jensen" "Robert Uhl" "Francis Burstall"
      "Arnaud BEAUD'HUIN" "Daniel V" "Albin Heimerson" "Alexander ter Weele"
      "Jeremy Firth" "aim" "liweitian" "Philipe Dallaire" "Travis Brown"
      "Divan Santana" "John C Haprian" "Pierrick Maillard" "Dardel Renaud"
      "Dardel Renaud" "Nils Grunwald" "hector" "Jean Morel" "Jos van Bakel"
      "slade" "dietrich ayala" "bacon totem" "Pierre Neidhardt"
      "18 anonymous")))
  "Crowdfunding campaigns. Each campaign is a title and tiers of names.")

(-> about-people ((list-of list)) null)
(defun about-people (people)
  "Write PEOPLE as name-over-handle rows."
  (spinneret:with-html
    (:ul :class "people"
         (dolist (person people)
           (:li
            (:span :class "name" (first person))
            (when (second person)
              (:span :class "handle" (second person)))))))
  nil)

(-> about-names ((list-of string)) null)
(defun about-names (names)
  "Write NAMES as one line each."
  (spinneret:with-html
    (:ul :class "names"
         (dolist (name names)
           (:li name))))
  nil)

(define-internal-page-command-global about ()
  (buffer "*About*")
  "Show the list of contributors."
  (spinneret:with-html-string
    (:nstyle
      '(h1
        :margin "0")
      '(section
        :margin-top "37px")
      '("section:first-of-type"
        :margin-top "17px")
      `(h2
        :margin "0 0 7px"
        :font-size "24px"
        :font-weight "600"
        :line-height "37px"
        :color ,theme:on-background-color)
      `(h3
        :margin "37px 0 7px"
        :font-size "17px"
        :font-weight "600"
        :line-height "24px"
        :color ,theme:on-background-color)
      '("section h3:first-of-type"
        :margin-top "0")
      '(.summary
        :max-width "80ch"
        :margin "17px 0 0"
        :font-size "17px"
        :line-height "24px")
      '("section > .summary:first-of-type"
        :margin-top "0")
      '(ul.people
        :list-style "none"
        :margin "0 -17px"
        :padding "0")
      '(ul.names
        :list-style "none"
        :margin "0 -17px"
        :padding "0")
      '("ul.people > li"
        :position "relative"
        :display "flex"
        :flex-direction "column"
        :justify-content "center"
        :box-sizing "border-box"
        :height "44px"
        :margin "0"
        :padding "0 17px")
      '("ul.people > li:has(span.handle)"
        :height "55px"
        :justify-content "flex-start"
        :padding "7px 17px")
      '("ul.names > li"
        :position "relative"
        :box-sizing "border-box"
        :height "44px"
        :margin "0"
        :padding "0 17px"
        :font-size "17px"
        :line-height "44px"
        :white-space "nowrap"
        :overflow "hidden"
        :text-overflow "ellipsis")
      `("ul.people > li:not(:last-child)::after, ul.names > li:not(:last-child)::after"
        :content ""
        :position "absolute"
        :right "17px"
        :bottom "0"
        :left "17px"
        :height "1px"
        :background-color ,theme:background-color-)
      '("ul.people span"
        :display "block"
        :overflow "hidden"
        :text-overflow "ellipsis"
        :white-space "nowrap")
      '("ul.people span.name"
        :height "24px"
        :font-size "17px"
        :line-height "24px")
      `("ul.people span.handle"
        :height "17px"
        :font-size "11px"
        :line-height "17px"
        :color ,theme:primary-color))
    (:h1 "About")
    (:section
     (:h2 "Contributors")
     (about-people *about-contributors*))
    (:section
     (:h2 "Supporters")
     (:p :class "summary"
         "Many thanks to all of our backers who've supported Nyxt development.")
     (:p :class "summary" "Thank you to NLnet for supporting Nyxt!"))
    (:section
     (:h2 "Crowdfunding")
     (:p :class "summary"
         "Thank you to all who have supported and made Nyxt possible!"))
    (dolist (campaign *about-campaigns*)
      (:section
       (:h2 (first campaign))
       (dolist (tier (rest campaign))
         (:h3 (first tier))
         (about-names (rest tier)))))))
