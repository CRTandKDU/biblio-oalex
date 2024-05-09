;;; cosma.el --- Support for the Cosma mind map tool

;; Copyright (C) 2023 jmc

;; Author: jmc
;; Maintainer: jmc
;; Created: Sunday, March 19, 2023
;; Keywords: terminologie, CELF
;; URL: 

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Change Log:
;;; Code:
(require 'cl)
(require 'helm-openalex)

(defvar cosma-zero-id 240000 "Start beyond 23:59:59 for id generation")

(defconst cosma-yaml-template "---
title: %s
id: %s
type: %s
tags:
  - Collège_Informatique
%s---

"
  "Template for root node and yaml headers of individual .md files.")

(defconst cosma-yaml-template--author "---
title: %s
id: %s
type: %s
oaid: %s
tags:
  - OpenAlex
%s---

"
  "Template for root work node and yaml headers of individual .md files.")

(defconst cosma-yaml-template--work "---
title: %s
id: %s
type: %s
oaid: %s
tags:
  - OpenAlex
%s---

"
  "Template for root author node and yaml headers of individual .md files.")


(defcustom cosma-dir ""
  "Directory for cosma Markdown files"
  :type '(directory)
  )

(defun cosma--newid ()
  "Generates a new unique id from the current day and incremented
global counter."
  (cl-incf cosma-zero-id)
  (let* ((now (decode-time (current-time)))
	 (str (format "%6d" cosma-zero-id))
	 (end (format "%2s%2s%2s" (substring str 0 2) (substring str 2 4) (substring str 4 6)))
	)
    (format "%04d%02d%02d%6s" (nth 5 now) (nth 4 now) (nth 3 now) end)))

(defun cosma--shortid (oaid)
  (car (last (string-split oaid "/"))))

;;; Ancillaries adapted from openalex.el in ORG-REF
(defun cosma--primary-topic (wrk)
  (let ((ptopic (oa-get wrk "primary_topic"))
	)
    (s-format "[${name}](${id}) in [${D}](${DID})/[${F}](${FID})/[${SUBF}](${SUBFID})"
	      (lambda (key data)
		(or (cdr (assoc key data)) ""))
	      `(("name" .	,(oa-get ptopic "display_name"))
		("id" .         ,(oa-get ptopic "id"))
		("D" .		,(oa-get ptopic "domain.display_name"))
		("DID" .	,(oa-get ptopic "domain.id"))
		("F" .		,(oa-get ptopic "field.display_name"))
		("FID" .	,(oa-get ptopic "field.id"))
		("SUBF" .	,(oa-get ptopic "subfield.display_name"))
		("SUBFID" .	,(oa-get ptopic "subfield.id"))
		)
	      )
    )
  )

(defun cosma--oa-authorships (wrk)
  "Return an author string for WRK.
The string is a comma-separated list of links to author pages in OpenAlex."
  (s-join ", " (cl-loop for author in (plist-get wrk :authorships)
			collect
			(format "[%s](%s)"
				(plist-get
				 (plist-get author :author)
				 :display_name)
				(plist-get
				 (plist-get author :author)
				 :id)))))

(defun cosma--oa-work (work)
  (let* ((work-data (request-response-data
		     (request (format work-api-url (cosma--shortid (cddr work)))
		       :sync t
		       :parser 'oa--response-parser
		       :params `(("mailto" . ,user-mail-address)
				 ("api_key" . ,oa-api-key)
				 ))))
	 )
    (insert (s-format "
<script src=\"https://cdn.plot.ly/plotly-2.32.0.min.js\" charset=\"utf-8\"></script>

[Web View](${landing_page_url}) | [Open Alex View](${oa-id})

## Authors
${authorships}

## Concepts
<div id=\"chart_concepts_${abbr-id}\" style=\"width:100%;max-width:700px\"></div>
<script>
 const data_${abbr-id} = [
     {
	 y: ['giraffes', 'orangutans', 'monkeys'],
	 x: [20, 14, 23],
	 type: 'bar',
	 orientation: 'h'
     }
 ];
 const layout_${abbr-id} = {
    title: 'Create a Static Chart',
    showlegend: false
 };
 TESTER = document.getElementById( 'chart_concepts_${abbr-id}' );
 Plotly.newPlot( TESTER, data_${abbr-id}, layout_${abbr-id}, {staticPlot: true} )
</script>


## Topics
${primary_topic}
"
		      (lambda (key data)
			(or (cdr (assoc key data)) ""))
		      `(("title" .		,(oa--title work-data))
			("oa-id" .		,(oa-get work-data "id"))
			("abbr-id" .            ,(cosma--shortid (oa-get work-data "id")))
			("publication_year" .	,(oa-get work-data "publication_year"))
			("landing_page_url" .	,(oa-get work-data "primary_location.landing_page_url"))
			("authorships" .	,(cosma--oa-authorships work-data))
			("primary_topic" .	,(cosma--primary-topic work-data))
			)
		      )
	    )
    )
  )


(defun cosma--oa-author-entries (works-data url &optional slice)
  "Get entries from WORKS-DATA.
Return an association list with key WORK-TITLE and value (COSMA-UUID . WORK-OAID).
"
  (let* ((meta (plist-get works-data :meta)) 
	 (per-page (plist-get meta :per_page))
	 (count (if slice slice (plist-get meta :count)))
	 (pages (/ count per-page))
	 (entries '())
	 purl)
    ;; if there is a remainder we need to get the rest
    (when (> (mod count per-page) 0) (cl-incf pages))
    (message "Generating: Count: %s, Pages: %s, Per-Page: %s\n" count pages per-page)
    
    ;; Now we have to loop through the pages
    (cl-loop for i from 1 to pages
	     do
	     (setq works-data (request-response-data
			       (request url
				 :sync t
				 :parser 'oa--response-parser
				 :params `(("mailto" . ,user-mail-address)
					   ("api_key" . ,oa-api-key)
					   ("page" . ,i))))
		   entries (append entries
				   (cl-loop for result in (plist-get works-data :results)
					    collect
					    (let ((wid (cosma--newid)))
					      ;; Returns an alist with fields of interest
					      (cons (s-format
						     "[[works:${wid}|${title}]], ${publication_year}"
						     (lambda (key data)
						       (or (cdr (assoc key data)) ""))
						     `(("title" . ,(oa--title result))
						       ("publication_year" . ,(oa-get result "publication_year"))
						       ("wid" . ,wid)
						       ))
						    (cons (oa--title result)
							  (cons wid (oa-get result "id"))))
					      )
					    ))))
    entries))
  

(defun cosma--oa-author-toc (oaid &optional slice)
  "Create and fill a buffer for a cosma table of content from Open
Alex, and return the TOC as an alist (WORK-TITLE . COSMA-UUID)."
  ;; Build TOC as an alist
  (let* ((toc nil)
	 (fn "AUTHOR")
	 (data (oa--author oaid))
	 (citations-image (oa--counts-by-year data))
	 (cited-by-count (plist-get data :cited_by_count))
	 (works-count (plist-get data :works_count))
	 (works-url (plist-get data :works_api_url))
	 (works-data (request-response-data
		      (request works-url
			:sync t
			:parser 'oa--response-parser
			:params `(("mailto" . ,user-mail-address)
				  ("api_key" . ,oa-api-key))))))
    ;; Init TOC buffer
    (with-current-buffer (get-buffer-create "*COSMA-TOC*")
      (erase-buffer)
      (set-buffer-file-coding-system 'utf-8)
      ;; Header
      (insert (format cosma-yaml-template--author
		      (format "%S" (plist-get data :display_name))
		      (cosma--newid)
		      fn
		      oaid
		      ""))
      ;; Headlines
      (insert "# Works\n")
      (let ((entries-alist (cosma--oa-author-entries works-data works-url slice)))
	(insert (s-join "\n" (mapcar (lambda (item)
				       (format "* %s" (car item)))
			      entries-alist)))
	(append-to-file (point-min) (point-max)
			(format "%s\\%s_%s.md" cosma-dir fn (cosma--shortid oaid)))
	(mapcar #'cdr entries-alist))
      )))


(defun cosma--toc (toc-title &optional save-as-file)
  "Generates a table of content `(term . uuid)` as an association
list and a root .md file from a properly formatted org buffer."
  ;; Init TOC buffer
  (with-current-buffer (get-buffer-create "*COSMA-TOC*")
    (erase-buffer)
    (set-buffer-file-coding-system 'utf-8)
    (insert (format cosma-yaml-template toc-title (cosma--newid) "liste" "")))

  ;; Build TOC as an alist
  (let ((toc nil)
	(ast (org-element-parse-buffer))
	)
    (org-element-map ast 'headline
      (lambda (hl)
	(if (= 3 (org-element-property ':level hl))
	    (let ((txt (buffer-substring (org-element-property ':contents-begin hl)
					 (org-element-property ':contents-end hl)))
		  (newid (cosma--newid))
		  (fn "terme")
		  )
	      ;; Append to TOC buffer
	      (with-current-buffer (get-buffer-create "*COSMA-TOC*")
		(insert
		 (format "%s [[inclut:%s]] *%s*\n"
			 (substring (format "%s" (org-element-property ':title hl)) 1 -1)
			 newid
			 (org-element-property ':title (org-element-property ':parent hl))
			 )))
	      ;; Append to alist
	      (push (cons (substring (format "%s" (org-element-property ':title hl)) 1 -1) newid) toc)
	      )
	  )
	)
      )
    ;; Save TOC
    (if save-as-file
	(with-current-buffer (get-buffer-create "*COSMA-TOC*")
	  (append-to-file (point-min) (point-max) (format "%s\\%s.md" dir toc-title))))
    toc
    )
  )

(defun cosma--links (txt toc config-alist)
  "Replaces the line beginning with key `prefix' of `config-alist'
with annotated lists of pointers (when present in org buffer).
`config-alist' specifies a minima `prefix', `separator' and
`link-type'."
  (let ((trim-left "[ \t\n\r]+")
	(trim-right "[ \t\n\r\\.]+"))
    (with-current-buffer (get-buffer-create "*COSMA-TMP*")
      (erase-buffer)
      (insert (format "%s" txt))
      (goto-char (point-min))
      (if (re-search-forward (cdr (assoc 'prefix config-alist)) nil t)
	  (let* ((refs (delete-and-extract-region (point) (point-at-eol)))
		 (terms (split-string refs (cdr (assoc 'separator config-alist)) t))
		 )
	    (insert
	     (format
	      "%s\n"
	      (string-join
	       (mapcar #'(lambda (key)
			   (let ((val (cdr (assoc (string-trim key trim-left trim-right) toc))))
			     ;; (debug val key (string-trim key trim-left trim-right))
			     (if val (format "%s [[%s:%s]]" key (cdr (assoc 'link-type config-alist)) val) key)))
		       terms)
	       ", "
	       ))
	     )
	    )
	)
      (buffer-substring-no-properties (point-min) (point-max))
      )
    )
  )

(defun cosma-export--oalex (oaid)
  (let* ((dir cosma-dir)
	 (work-api-url "https://api.openalex.org/works/%s")
	 (toc (cosma--oa-author-toc oaid))
	 )
    (dolist (work toc)
      ;; WORK is a list (TITLE UUID . OAID)
      (let ((fn "WORK"))
	(with-current-buffer (get-buffer-create "*COSMA*")
	  (erase-buffer)
	  (set-buffer-file-coding-system 'utf-8)
	  ;; Add header
	  (insert (format cosma-yaml-template--work
			  (format "%S" (car work))
			  (cadr work)
			  fn
			  (cddr work)
			  ""))
	  ;; Content
	  (cosma--oa-work work)
	  ;; Save individual WORK Markdown file
	  (append-to-file (point-min) (point-max)
			  (format "%s\\%s_%s.md" dir fn (cadr work)))
	  )
	)
      )
    )
  )
    
	    
(defun cosma-export--org ()
  (interactive)
  (let ((ast (org-element-parse-buffer))
	(buf (current-buffer))
	(dir cosma-dir)
	)

    ;; TOC matters
    (let ((toc (cosma--toc (buffer-name)))
	  )
      ;; Terms are 3rd-level headlines in the master org-file
      (org-element-map ast 'headline
	(lambda (hl)
	  (if (= 3 (org-element-property ':level hl))
	      (let* ((txt (buffer-substring (org-element-property ':contents-begin hl)
					    (org-element-property ':contents-end hl)))
		     (key (substring (format "%s" (org-element-property ':title hl)) 1 -1))
		     (newid (cdr (assoc key toc)))
		     (fn (format "%s" (or (org-element-property ':SKOSTYPE hl) "terme")))
		     )

		;; Create new record file
		(with-current-buffer (get-buffer-create "*COSMA*")
		  (erase-buffer)
		  (set-buffer-file-coding-system 'utf-8)
		  (insert
		   (format
		    cosma-yaml-template
		    (substring (format "%s" (org-element-property ':title hl)) 1 -1)
		    newid
		    fn
		    (format "  - %s\n"
			    (substring
			     (format "%s" (org-element-property ':title (org-element-property ':parent hl))) 1 -1))))
		  (insert
		   (format "%s\n"
			   (let ((transtxt txt))
			     (dolist (transform
				      '(((prefix . "Généralise : ") (separator . ", ") (link-type . "generalise"))
					((prefix . "Spécialise : ") (separator . ", ") (link-type . "specialise"))
					((prefix . "Voir aussi : ") (separator . ", ") (link-type . "voir_aussi"))
					;; ((prefix . "Schémas : ") (separator . ", ") (link-type . "schema"))
					)
				      transtxt)
			       (setq transtxt (cosma--links transtxt toc transform))))))

		  (append-to-file (point-min) (point-max) (format "%s\\%s_%s.md" dir fn newid))
		  )
		)
	    )
	  )
	)

    )
    )
  )

(provide 'cosma)
;;; cosma.el ends here
