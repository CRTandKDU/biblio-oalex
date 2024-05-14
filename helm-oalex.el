;;; helm-oalex.el --- Lookup and import bibliographic entries from Open Alex  

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
;;  Again, the invaluable John Kitchin [[https://wikemacs.org/wiki/How_to_write_helm_extensions]]

;;; Code:
(require 'helm-openalex)
(require 'cosma)

;; Crude unplugging of 'ivy".
(defun ivy-more-chars () nil)

(defun helm-oalex--cosmo (oaid)
  ;; WARNING: Clean directory
  (shell-command (format "del %s\\*.md" cosma-dir))
  (cosma-export--oalex oaid)
  )

(defun oalex-helm--authors-source (query)
  (list
   (list '(name . "Open Alex: Authors")
	 (cons 'candidates (lambda () (oa--author-candidates query)))
	 '(action . (("Message" . (lambda (candidate)
				    (message
				     "%s"
				     (get-text-property 0 'oaid (helm-get-selection nil 'withprop)))
				    nil))
		     ("Org-mode" . (lambda (candidate)
				     (oa--author-org
				      (get-text-property 0 'oaid (helm-get-selection nil 'withprop))
				      500)))
		     ("Cosmoscope" . (lambda (candidate)
				       (helm-oalex--cosmo
					(get-text-property 0 'oaid (helm-get-selection nil 'withprop)))))
		     ("Browse" . (lambda (candidate)
				   (browse-url (get-text-property 0 'oaid (helm-get-selection nil 'withprop)))))
		     )
		  )
	 )
   )
  )

(defun helm-oalex (query)
  (interactive)
  (helm :sources (oalex-helm--authors-source query)))

(provide 'helm-oalex)
;;; biblio-oalex.el ends here
