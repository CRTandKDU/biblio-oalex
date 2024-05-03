;;; biblio-citeproc.el --- Use citproc to extend biblio's action set -*- lexical-binding: t -*-

;; Copyright (c) 2024  Jean-Marie Chauvet

;; Author: Jean-Marie Chauvet, inspired by Clément Pit-Claudel and Andras Simonyi
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

;;; Code:
(require 'biblio-core)
(require 'biblio-doi)
(require 'citeproc)

(defcustom biblio-citeproc-locale-dir ""
  "Directory to search for .xml locale file(s) required for CSL"
  :type '(directory)
  :group 'biblio
  )

;; CSL Styles: See also[[https://github.com/citation-style-language/styles/tree/master]]
(defcustom biblio-citeproc-csl-styles
  nil
  "Alist for CSL style file choice"
  :type '(sexp)
  :group 'biblio
  )

(defconst biblio-citeproc--temp-buffer-name "*FOO*")

;; Supersedes original: adds ".buffer" extension in pcase
(defun biblio-citeproc--hash-itemgetter-from-any-with-buffer (file-or-files &optional no-sentcase-wo-langid)
  "Return a getter for FILE-OR-FILES in any supported format.
The format is determined on the basis of file extensions.
Supported formats:
- CSL-JSON (.json extension) the recommended native format;
- BibTeX/biblatex (.bib or .bibtex extension),
- org-bibtex (.org extension).
If NO-SENTCASE-WO-LANGID is non-nil then title fields in items
without a `langid' field are not converted to sentence-case."
  (let ((files (if (listp file-or-files)
		   file-or-files
		 (list file-or-files)))
	(cache (make-hash-table :test #'equal))
	(bt-entries (make-hash-table :test #'equal))
	(bt-strings (make-hash-table :test #'equal)))
    (dolist (file files)
      (pcase (file-name-extension file)
        ("json"
         (let ((json-array-type 'list)
               (json-key-type 'symbol))
           (dolist (item (json-read-file file))
             (puthash (cdr (assq 'id item)) item cache))))
        ((and (or "bib" "bibtex") ext)
         (with-temp-buffer
	   (insert-file-contents file)
	   (bibtex-set-dialect (if (string= ext "bib") 'biblatex 'BibTeX) t)
	   (citeproc-itemgetters--parsebib-buffer bt-entries bt-strings)))
	("buffer"
	 (with-current-buffer (file-name-sans-extension file)
	   (bibtex-set-dialect 'biblatex t)
	   (citeproc-itemgetters--parsebib-buffer bt-entries bt-strings)))
	("org"
	 (org-map-entries
	  (lambda ()
	    (-when-let (key-w-entry (citeproc-bt-from-org-headline))
	      (condition-case err
		  (puthash (car key-w-entry) (citeproc-blt-entry-to-csl
					      (cdr key-w-entry))
			   cache)
		(error
		 (user-error
		  "Couldn't parse the bib(la)tex entry with key '%s', the error was: %s" 
		  (car key-w-entry) err)))))
	  t (list file)))
        (ext
         (user-error "Unknown bibliography extension: %S" ext))))
    (maphash
     (lambda (key entry)
       (condition-case err
	   (puthash key (citeproc-blt-entry-to-csl entry nil no-sentcase-wo-langid)
		    cache)
	 (error
	  (user-error
	   "Couldn't parse the bib(la)tex entry with key '%s', the error was: %s"
	   key err))))
     bt-entries)
    (lambda (x)
      (pcase x
	('itemids
	 (hash-table-keys cache))
	((pred listp) (mapcar (lambda (id)
				(cons id (gethash id cache)))
			      x))
	(_ (error "Unsupported citeproc itemgetter retrieval method"))))))

(defun biblio-citeproc--as-references (buf csl bibids)
  "Compute plain text citation for each biblatex entry in buffer
BUF, a string with template '<emacs buffer name>.buffer', whose key
is in BIBIDS. Use style file CSL.

Returns a list of text references as strings."
  (let*
    ((lg (citeproc-locale-getter-from-dir biblio-citeproc-locale-dir))
     (style (citeproc-create-style csl lg))
     (ig (biblio-citeproc--hash-itemgetter-from-any-with-buffer buf))
     (items (funcall ig bibids)))
    (mapcar
     (lambda (item) (citeproc-render-item (cdr item) style 'bib 'plain))
     items)))

(defun biblio-citeproc--doi-insert-bibtex (doi csl biblio-buf forward-buf target-buffer)
  "Insert BibTeX entry matching DOI at point in TARGET-BUFFER.
DOI		: the doi string from the selected metadata
CSL		: the .csl style file
BIBLIO-BUF	: the original biblio search-result buffer (not used)
FORWARD-BUF	: where to insert the plain text reference
TARGET-BUFFER	: the intermediate work buffer with all biblatex entries
"
  (biblio-doi-forward-bibtex
   (biblio-cleanup-doi doi)
   (lambda (result)
     (biblio-doi--insert
      (biblio-format-bibtex result biblio-bibtex-use-autokey)
      target-buffer)
     (let ((hentries (parsebib-collect-bib-entries)))
       (with-current-buffer forward-buf
	 (dolist (reference
		  (biblio-citeproc--as-references
		   (concat biblio-citeproc--temp-buffer-name ".buffer")
		   csl
		   (hash-table-keys hentries)))
	   (insert (format "%s\n" reference)))))
     (kill-buffer)
     )))

(defun biblio-citeproc--insert-reference-text (metadata)
  "Insert the selected entry as a plain text reference in the target
buffer.

This proved more difficult to implement than it appears.

  1. A CSL style for rendering the reference in plain text is
  selected. The style keyword is chosen from a customizabe
  mapping of keys to CSL-formatted files in the custom variable
  `biblio-citeproc-csl-styles'.

  2. The DOI sever is queried for a biblatex entry which, on
  return, is inserted in the cleaned up buffer '*FOO*'. This
  insertion is performed in the lambda call-back in function
  `biblio-citeproc--doi-insert-bibtex'.

  3. The biblatex entry in this buffer is parsed (using the
  `parsebib' library) to finally pass the list of keys and the
  buffer containing the biblatex entry to `citeproc' for
  formatting.

  4. A slightly modified citeproc primitive, which reads from a
  buffer rather than from a file provided its name has the
  .buffer suffix, formats these biblatex entries in the *FOO*
  buffer whose keys are in the passed list, according to the
  passed CSL file. Each plain text reference thus obtained is
  inserted at point in the target buffer. The *FOO* buffer is
  then killed, and the buffer should be set back to the biblio
  search results.

There is surely a better way than translating metadata to bibtex
to citeproc reference.
"
  (let ((csl (completing-read "Choose CSL style: "
			      biblio-citeproc-csl-styles nil t))
	(biblio-buf (current-buffer))
	(forward-buf biblio--target-buffer ))
    (with-current-buffer (get-buffer-create biblio-citeproc--temp-buffer-name)
      (erase-buffer)
      (bibtex-mode)
      (biblio-citeproc--doi-insert-bibtex
       (biblio-alist-get 'doi metadata)
       (cdr (assoc csl biblio-citeproc-csl-styles))
       biblio-buf
       forward-buf
       (current-buffer))
      )))


(add-to-list 'biblio-selection-mode-actions-alist
             '("Insert Reference in plain text, according to selected CSL" .
               biblio-citeproc--insert-reference-text))

(provide 'biblio-citeproc)
;;; biblio-citeproc.el ends here
