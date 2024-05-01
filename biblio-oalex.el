;;; biblio-oalex.el --- Lookup and import bibliographic entries from Open Alex -*- lexical-binding: t -*-

;; Copyright (c) 2024  Jean-Marie Chauvet

;; Author: Jean-Marie Chauvet, inspired by Clément Pit-Claudel 
;; URL: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Lookup and download bibliographic records from Open Alex.
;;
;; This file implements a backend for the `biblio' package (which see for more
;; documentation).

;;; Code:

(require 'biblio-core)
(require 'biblio-doi)

(defun biblio-oalex--forward-bibtex (metadata forward-to)
  "Forward BibTeX for OALEX entry METADATA to FORWARD-TO."
  (biblio-doi-forward-bibtex (biblio-alist-get 'doi metadata) forward-to))

(defun biblio-oalex--format-author (authorship)
  (let-alist authorship
    (biblio-join " " .author.display_name)))

(defun biblio-oalex--extract-interesting-fields (item)
  "Prepare a OALEX search result ITEM for display."
  (let-alist item
    (list (cons 'year .publication_year)
	  (cons 'type .type)
	  (cons 'authors (seq-map #'biblio-oalex--format-author .authorships))
	  (cons 'title .display_name)
	  (cons 'url .primary_location.landing_page_url)
	  (cons 'container .primary_location.source.display_name)
	  (cons 'open-access-status .open_access.oa_status)
	  (cons 'category .primary_topic.display_name)
	  ;;
	  (cons 'doi .ids.doi)
	  (cons 'open-alex .id)
	  )
    ))

(defun biblio-oalex--parse-search-results ()
  "Extract search results from OALEX response."
  (biblio-decode-url-buffer 'utf-8)
  (let-alist (json-read)
    (display-warning 'biblio-oalex (format "OALEX %d hits." .meta.count))
    (seq-map #'biblio-oalex--extract-interesting-fields .results))
  )

(defun biblio-oalex--url (query)
    "Create a OALEX url to look up QUERY.
ENDPOINT can be works, authors, sources, institutions, concepts,
publishers, or funders."
    (format "https://api.openalex.org/works?search=%s" (url-encode-url query))
  )

;;;###autoload
(defun biblio-oalex-backend (command &optional arg &rest more)
  "A OALEX backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    (`name "OALEX")
    (`prompt "OALEX query: ")
    (`url (biblio-oalex--url arg))
    (`parse-buffer (biblio-oalex--parse-search-results))
    (`forward-bibtex (biblio-oalex--forward-bibtex arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-oalex-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-oalex-backend)

;;;###autoload
(defun biblio-oalex-lookup (&optional query)
  "Start a OALEX search for QUERY, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-oalex-backend query))


(provide 'biblio-oalex)
;;; biblio-oalex.el ends here
