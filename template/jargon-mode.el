;;; jargon-mode.el --- minor mode for editing the Jargon File master.

(defvar jargon-mode nil
   "Control variable for jargon minor mode.");
(make-variable-buffer-local 'jargon-mode)
(set-default 'jargon-mode nil)

(defvar jargon-mode-map nil)
(if jargon-mode-map
    nil
  (setq jargon-mode-map (make-sparse-keymap))
  (define-key jargon-mode-map "\C-xt" 'jargon-attribute);
  (define-key jargon-mode-map "\C-c\C-e" 'jargon-edit-entry)
  (define-key jargon-mode-map "\C-c\C-f" 'jargon-find-entry)
  (define-key jargon-mode-map "\C-c\C-n" 'jargon-next-entry)
  (define-key jargon-mode-map "\C-c\C-p" 'jargon-prev-entry)
  (define-key jargon-mode-map "\C-c\C-d" 'jargon-delete-entry)
  (define-key jargon-mode-map "\C-c\C-r" 'jargon-rename-entry)
  (define-key jargon-mode-map "\C-c\C-l" 'jargon-make-reference)
  (define-key jargon-mode-map "\C-c\C-i" 'jargon-make-firstterm)
  (define-key jargon-mode-map "\M-q" 'jargon-fill)
  )

(or (assq 'jargon-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(jargon-mode " Jargon-XML") minor-mode-alist)))

(or (assq 'jargon-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons
	   (cons 'jargon-mode jargon-mode-map)
	   minor-mode-map-alist)))

(defun jargon-mode (&optional arg)
   "Helper functions for editing the Jargon File.  This minor mode
extends SGML mode.

\\[jargon-find-entry]	find the named Jargon File entry

Find an entry by name.  Return error if there is no such entry.  Useful for
checking whether an entry exists without committing to edit it.

\\[jargon-edit-entry]	edit the named Jargon File entry

This command does all necessary things to open an edit of an existing or
new entry.

\\[jargon-rename-entry]	rename the named Jargon File entry to a new name

Adds a rename history entry.  Doesn't actually hack the name field.

\\[jargon-attribute]	add an attribution to the named entry

Call this from within a mailbox file.  It takes an entry name as
argument and tries to add an attribtion line based on the sender
name and date.

For an existing entry, it goes to that entry and adds a revision history
entry referencing the current version.  It does *not* check for
an existing revision history entry, because the version number might
have been bumped.

\\[jargon-delete-entry]	delete the named entry

Actually, move it to the chaff file.

There are a few more convenience functions:

\\[jargon-make-reference]	prompt for entry, create a reference to it

\\[jargon-make-firstterm]	insert a first-term reference to a given term

\\[jargon-next-entry]	go to next entry

\\[jargon-prev-entry]	go to previous entry

"""
  (interactive "P")
  (setq jargon-mode
	(if (null arg) (not jargon-mode)
	  (> (prefix-numeric-value arg) 0))))

;;
;; Environment queries
;;

(defun jargon-entry-file (tag)
  "Visit the lexicon file containing a given entry."
  ;; It's all one file now.
  (find-file "jargon.xml")
  (setq jargon-mode t)
  )

(defun fetch-info (file field &optional delete)
  "Fetch, from a given FILE, a given FIELD."
  (save-excursion
    (find-file file)
    (goto-char (point-min))
    (prog1
	(and (re-search-forward field nil nil)
	     (buffer-substring (match-beginning 1) (match-end 1)))
      (if delete
	  (kill-buffer (current-buffer))))))

(defun jargon-current-version ()
  (fetch-info "jargon.xml" "<!ENTITY version  *\'\\([0-9.]*\\)\'>"))

(defun replace-in-string (str x y)
  (save-excursion
    (set-buffer (get-buffer-create "*scratch*"))
    (erase-buffer)
    (insert str)
    (goto-char 0)
    (replace-string x y)
    (buffer-string)))

(defun jargon-translate (str table)
  "Translate characters in a string."
  (mapcar
   (function (lambda (x)
	       (save-match-data
		 (while (string-match (car x) str)
		   (setq str (replace-match (cadr x) t t str))))))
   table)
  str)

(defun jargon-xmlize (str)
  "Hack a string into valid XML."
  (jargon-translate str '(("&" "&amp;") ("<" "&lt;") (">" "&gt;"))))

(defun jargon-normalize (str)
  "Hack a string into a valid XML attribute value"
  (jargon-translate str '((" " "-") 
		      ("\n" "-") 
		      ("/" "-")
		      ("\\\\" "-")
		      ("!" "-")
		      ("\\." "-")
		      ("," "-")
		      ("'" "-")
		      )))

(defun jargon-renormalize ()
  "Regenerate id attributes on glossary entries."
  (interactive)
  (while (re-search-forward "<glossentry\\([^>]*\\)><glossterm>\\([^>]*\\)</glossterm>" nil t)
    (replace-match (concat " id='" (jargon-normalize (match-string 2))  "'") t t nil 1)))

;;
;; Define the Jargon File sort order
;;
;; This is important, because the binary-search code that computes where
;; to put new entries depends on it.
;;
;; Our desired sort order differs from ASCII sort in that we want case
;; variants to sort together. 

(defun jargon-sortkey (word)
  "Generate the sort key corresponding to a word."
  (setq word (downcase word))
  (if (= (aref word 0) ?-)
      (setq word (substring word 1)))
  word)

(defun jargon-headword-< (p q)
  "Compare strings according to the Lexicon's dictionary sort order."
  (let ((lp (jargon-sortkey p)) (lq (jargon-sortkey q)))
	(if (string= lp lq)
	    (string< p q)
	  (string< lp lq))))

(defun jargon-check-sort ()
  "Check that the entries are in proper sort order."
  (interactive)
  (goto-char (point-min))
  (jargon-next-entry)
  (unwind-protect
      (while t
	(let ((this (jargon-this-headword)) (next (jargon-next-headword)))
	  (if (not (jargon-headword-< this next))
	      (error "%s is out of order with respect to %s" this next)))
	(jargon-next-entry))))

;;
;; Navigation
;;

(defun jargon-start-of-entry ()
  (if (not (looking-at "^<glossentry"))
      (re-search-backward "^<glossentry")))

(defun jargon-end-of-entry ()
  (if (not (looking-at "^</glossentry>"))
      (re-search-forward "^</glossentry>\n")))

(defun jargon-next-entry ()
  "Go to start of next entry."
  (interactive)
  (next-line 1)
  (if (re-search-forward "<glossentry" nil t)
      (beginning-of-line)
    (goto-char (1- (point-max)))
    (re-search-backward "^$")
    (forward-char 1)))

(defun jargon-prev-entry () 
  "Go to start of previous entry"
  (if (re-search-backward "<glossentry" nil t)
      (beginning-of-line)
    (goto-char (1+ (point-min)))
    (re-search-forward "^$")
    (backward-char 1)))

(defun jargon-this-headword ()
  "Get the headword of the current entry."
  (save-excursion
    (if (not (looking-at "<glossentry"))
	(re-search-backward "<glossentry"))
    (re-search-forward "<glossterm[^>]*>\\([^<]*\\)</glossterm>")
    (match-string 1)))

(defun jargon-prev-headword () 
  "Get the headword of the previous entry"
  (save-excursion
    (jargon-prev-entry)
    (jargon-this-headword)))

(defun jargon-next-headword () 
  "Get the headword of the next entry"
  (save-excursion
    (jargon-next-entry)
    (jargon-this-headword)))

(defun jargon-goto-entry (tag) 
  "Go to an entry, or the next one up if it doesn't exist."
  (interactive "sGo to headword: ")
  ;;(jargon-entry-file tag)
  (let (begin end pivot done cword)
    (goto-char (point-min))
    (jargon-next-entry)
    (forward-line -1)
    (setq begin (point))
    (goto-char (point-max))
    (jargon-prev-entry)
    (forward-line 1)
    (setq end (point))
    (prog1
	(catch t
	  (while t
	    (goto-char (/ (+ begin end) 2))
	    (if (or (= (point) begin) (= (point) end))
		(throw t nil)
	      (setq cword (jargon-this-headword))
	      (cond ((jargon-headword-< cword tag) (setq begin (point)))
		    ((jargon-headword-< tag cword) (setq end (point)))
		    (t (throw t t))))))
      (end-of-line)
      (jargon-start-of-entry))))

(defun jargon-find-entry (entry) 
  "Find entry with given headword, avoiding XML gotchas."
  (interactive "sFind headword: ")
  (jargon-goto-entry (jargon-xmlize entry)))

;;
;; Editing
;;

(defun jargon-make-reference (str)
  "Insert a cross-reference to a given term."
  (interactive "sMake reference to: ")
  (insert (format "<glossterm>%s</glossterm>" str str)))

(defun jargon-make-firstterm (str)
  "Insert a first-term reference to a given term."
  (interactive "sMake first-term reference to: ")
  (insert (format "<emphasis role='firstterm'>%s</emphasis>" str str)))

(defun jargon-fill ()
  (interactive)
  (save-excursion
    (let ((start 
	   (progn 
	     (if (not (search-forward "<para>" (save-excursion (end-of-line) (point)) t))
		      (progn
			(search-backward "<para>") 
			(beginning-of-line)))
	     (point)))
	  (end
	   (progn 
	     (search-forward "</para>")
	     (beginning-of-line)
	     (forward-line 1)
	     (point))))
      (fill-region-as-paragraph start end))))

(defun jargon-edit-entry (tag comment)
  "Edit an entry."
  (interactive "sEdit entry: \nsComment: ")
  (if (string= tag "")
      (setq tag (jargon-this-headword)))
  (jargon-goto-entry tag)
  (if (string= (jargon-this-headword) tag)
      (jargon-append-revision (jargon-current-version) "Changed" comment)
    (jargon-new-entry tag comment)))

(defun jargon-rename-entry (tag newname)
  "Rename an entry."
  (interactive "sRename entry: \nsNewname: ")
  (if (string= tag "")
      (setq tag (jargon-this-headword)))
  (jargon-goto-entry tag)
  (if (string= (jargon-this-headword) tag)
      (jargon-append-revision 
       (jargon-current-version) 
       (format "Rename: '%s' -> '%s'" tag newname))
    (error "no such entry")))

(defun jargon-new-entry (tag comment)
  "Insert a new entry before the current one,"
  (jargon-start-of-entry)
  (if (/= (aref (jargon-sortkey (jargon-this-headword)) 0)
	      (aref (jargon-sortkey (jargon-prev-headword)) 0))
      (progn
       (re-search-backward "^<glossdiv")
       (forward-line -1)))
  (insert 
     "<glossentry id='" (jargon-normalize tag) "'><glossterm>" tag "</glossterm>\n"
     "<abbrev>\n<emphasis role='grammar'>n.</emphasis>\n</abbrev>\n"
     "<glossdef>\n   <para></para>\n</glossdef>\n</glossentry>\n\n")
  (search-backward "<glossentry")
  (jargon-append-revision (jargon-current-version) "Added" comment)
  (search-forward "<para>"))

(defun jargon-delete-entry (entry comment)
  "Remove entry with given headword."
  (interactive "sEntry to be deleted: \nsComment: ")
  (if (string= entry "")
      (setq entry (jargon-this-headword)))
  (if (not (jargon-find-entry-or-die entry))
      (error "No such entry.")
    (save-excursion
      (forward-line -1)
      (let* ((start (point)) 
	     (end 
	      (progn
		(jargon-end-of-entry)
		(point))))
	(kill-region start end)
	(basic-save-buffer)
	(find-file "chaff.xml")
	(goto-char (point-max))
	(search-backward "\n</glossdiv>")
	(if (save-excursion (not (re-search-backward (concat "<glossdiv>.*" (jargon-current-version) ".*</title>") nil t)))
	    (insert "<glossdiv><title>Deleted before " (jargon-current-version) ".*</title>\n<glossdiv>\n"))
	(yank)
	(jargon-start-of-entry)
	(jargon-append-revision (jargon-current-version) "Deleted" comment)
	(basic-save-buffer)
	;;(pop-to-buffer "chaff.xml")
	))))

(defun jargon-internal-find-entry (entry)
  "Find entry with given headword, error if-nonexistent."
  (goto-char (point-min))
  (prog1
      (let ((case-fold-search t))
	(re-search-forward (concat 
			    "^<glossentry id='" (jargon-normalize entry) "'><glossterm>" 
			    (regexp-quote (jargon-xmlize entry))
			    "</glossterm>")))
    (beginning-of-line)))

(defun jargon-find-entry-or-die (entry)
  "Find entry with given headword, error if-nonexistent."
  (jargon-entry-file entry)
  (jargon-internal-find-entry entry))

(defun jargon-kill-entry ()
  "Delete current entry into the kill ring -- must be at start of entry."
   (kill-region (point)
		(progn 
		  (jargon-end-of-entry)
		  (forward-line 1)
		  (point))))

(defun jargon-undelete-entry (entry)
  "Undo deletion of given entry."
  (interactive "sEntry to be undeleted: ")
  (find-file "chaff.xml")
  (if (string= entry "")
      (setq entry (jargon-this-headword)))
  (jargon-internal-find-entry entry)
  (jargon-kill-entry)
  (basic-save-buffer)
  (jargon-entry-file entry)
  (jargon-goto-entry entry)
  (jargon-next-entry)
  (yank)
  (jargon-prev-entry)
  (jargon-append-revision (jargon-current-version) "Undeleted" "")
  (basic-save-buffer)
  )

(defun jargon-move-entry (from to)
   "Move entry FROM to just after entry TO."
   (interactive "sFrom entry: \nsAfter entry: ")
   (if (not (jargon-find-entry to))
       (error "No such to entry"))
   (if (not (jargon-find-entry from))
       (error "No such from entry"))
   (jargon-kill-entry)
   (jargon-find-entry-or-die to)
   (jargon-next-entry)
   (yank)
   )

;;
;; Handling jargon mail
;;

(defun jargon-transform ()
  "Crunch contents of a mail message into proper form for the jargon master.
Does the right thing with string quotes, ampersand, medial and final ellipses."
  (goto-char (point-min))
  (replace-regexp " \"\\([a-z]\\)" " <quote>\\1")
  (goto-char (point-min))
  (replace-regexp "^\"\\([a-z]\\)" "<quote>>\\1")
  (goto-char (point-min))
  (replace-regexp "\\([a-z?!.,]\\)\" " "\\1</quote> ")
  (goto-char (point-min))
  (replace-regexp "\\([a-z?!.,]\\)\"$" "\\1</quote>>")
  (goto-char (point-min))
  (replace-regexp "\\.\\.\\.\\.$" "&endellipsis;")
  (goto-char (point-min))
  (replace-regexp "\\.\\.\\." "&ellipsis;")
  (mapcar
   (function (lambda (x)
	       (goto-char (point-min))
	       (replace-string (car x) (cadr x))))
   '(("<" "&lt;") (">" "&gt;") ("&" "&amp;")))
  )

(defun jargon-get-posting-date () 
  "Try to extract a date from the mail message we're in."
  (save-excursion
    (re-search-forward "^$" nil t)
    (re-search-backward "^Date: [A-Z][a-z][a-z], \\([0-9][0-9]?\\) \\([A-Z][a-z][a-z]\\) \\([0-9]*\\) ")
    (let ((day (match-string 1))
	  (month (match-string 2))
	  (year (match-string 3)))
      (if (= (length day) 1)
	  (setq day (concat "0" day)))
      (if (= (length year) 2)
	  (setq year (concat "19" year)))
      (concat day " " month " " year)
      )))

(defun jargon-narrow-to-message nil 
  "Narrow scope of searches to current message in a mailbox file"
  (save-excursion
    (let (beg end)
      (if (not (re-search-backward "\n\nFrom " nil t))
	  (goto-char (point-min)))
      (setq beg (point))
      (forward-line 1)
      (if (not (re-search-forward "\n\nFrom " nil t))
	  (goto-char (point-max)))
      (forward-line -1)
      (setq end (point))
      (narrow-to-region beg end))))

(defun jargon-get-attribution ()
  (save-excursion
    (let ((name))
      (jargon-narrow-to-message)
      (goto-char (point-min))
      (mapcar
       (lambda (x)
	 (if (re-search-forward x nil t)
	     (progn
	       (if (not (fboundp 'mail-extract-address-components))
		   (load "mail-extr"))
	       (setq name (mail-extract-address-components
			   (buffer-substring
			    (point)
			    (save-excursion (end-of-line) (point))) )))
	   ))
       '("^Sender: " "^From: " "^Path: "))
      (widen)
      (concat
       "from "
       (car name)
       " <"
       (car (cdr name))
       ">, "
       (jargon-get-posting-date)))))

(defun jargon-attribute (entry) 
  "Generate an attribution for a term from current message."
  (interactive "sEntry to attribute: ")
  (let ((attribution (jargon-get-attribution)))
    (jargon-entry-file entry)
    (jargon-mark-entry entry attribution)))

(defun jargon-mark-entry (entry string) 
  "Append credit to entry's cite list."
  (if (jargon-find-entry-or-die entry)
      (progn
	(search-forward "</glossentry>\n")
	(forward-line -1)
	(insert "<!-- " string " -->\n")
	(message "Marking %s with %s" entry string)
	(sit-for 0)
	)))

;;
;; Revision histories
;;

(defun jargon-make-revision-at-point (rev tag &optional comment)
  "Create a new revision at point."
  (if (and comment (not (string= comment ""))) 
      (setq tag (concat tag ": " comment)))
  (insert
   "<revision>\n"
   "  <revnumber>" rev "</revnumber>\n"
   "  <date>entered: " (current-time-string) "</date>\n"
   "  <revremark>" tag "</revremark>\n"
   "</revision>\n"))

(defun jargon-append-revision (rev tag comment)
  "Mark the current entry with the given revision and tag."
  (if (not (looking-at "<glossentry")) 
      (jargon-start-of-entry))
  (search-forward "<glossdef>")
  (beginning-of-line)
  (forward-line -1)
  (if (not (looking-at "</revhistory>"))
      (progn
	(forward-line 1)
	(insert "<revhistory>\n</revhistory>\n")
	(forward-line -1)))
  (jargon-make-revision-at-point rev tag comment)
  (search-backward "</revremark>"))

(defun jargon-canonicalize-revision-number (x)
  "Canonicalize a revision level to a 6-digit string."
  (if (= (length x) 5)
      (concat (substring x 0 4) "0" (substring x 4))
    x))

(defun jargon-get-revision-level ()
  "From start of revision, what's its level number?"
    (and 
     (looking-at "<revision>\n *<revnumber>\\([0-9.]*\\)</revnumber>\n")
     (match-string 1)))

(defun jargon-revision-ge (x y)
  "Compare revision levels in X.Y.Z order."
  (not (string< 
	(jargon-canonicalize-revision-number y)
	(jargon-canonicalize-revision-number x) 
	)))

(defun jargon-find-revision (rev &optional headword)
  "Go to start of revision with specified REV, or where to insert it.
Return t if we found an entry with matching revision, nil otherwise."
  (and (or (not headword) (jargon-goto-entry headword))
       (if (search-forward "<revhistory>\n" 
			   (save-excursion (jargon-end-of-entry) (point)) t)
	   ;; There's a history; search for an entry with a matching rev number
	   (catch t
	     (while t
	       (cond ((not (jargon-get-revision-level)) 
		      (throw t nil))
		     ((or (jargon-revision-ge rev (match-string 1)))
		      (throw t t))
		     (t (search-forward "</revision>\n")))))
	 ;; No revision history, go to the right place to start one
	 (progn
	   (search-forward "<glossdef>\n")
	   (forward-line -1)
	   nil))))

(defun jargon-get-revremark (&optional rev headword)
  "Extract the content of a revision-history entry."
  (save-excursion
    (and (or (not rev) (jargon-find-revision rev headword))
	 (or (not rev) (string= (jargon-get-revision-level) rev))
	 (let* ((begin (re-search-forward "<revremark>" nil t))
		(end (re-search-forward "</revremark>" nil t)))
	   (buffer-substring begin (- end 12))))))

(defun jargon-set-revremark (content &optional rev headword)
  "Change the content of a revision-history entry."
  (save-excursion
    (and (or (not headword) (jargon-goto-entry headword))
       (if (jargon-get-revremark rev)
	   (replace-match content t t nil 1)
	 (jargon-find-revision rev)
	 (jargon-make-revision-at-point rev content)))))

;;
;; Release generation
;;

(defun basic-patch-sizes ()
  "Patch the sizes in the version history entry nearest the magic cookie."
   (interactive)
   (let (entries lines words chars (version (jargon-current-version)))
     (find-file "jargon.xml")
     (save-excursion
       (if (not (zerop (shell-command "make size")))
	   (error "Size computation failed"))
       (set-buffer "*Shell Command Output*")
       (beginning-of-buffer)
       (re-search-forward "\\([0-9]+\\)[ \n]+\\([0-9]+\\)[ \n]+\\([0-9]+\\)[ \n]+\\([0-9]+\\)" nil t)
       (setq entries (match-string 1))
       (setq lines (match-string 2)) 
       (setq words (match-string 3))
       (setq chars (match-string 4)))
     ;; Back in the master file...we want the search to error out if it fails
     (search-forward "<!-- TABLE MAGIC -->")
     (beginning-of-line)
     (forward-line -1)
     (let* ((now (current-time-string))
	    (date (concat (substring now 8 11) (substring now 4 8) (substring now 20)))
	    (sizes (concat "<row>\n"
			 "<entry>" version "</entry>\n"
			 "<entry>" date "</entry>\n"
			 "<entry>" lines "</entry>\n"
			 "<entry>" words "</entry>\n"
			 "<entry>" chars "</entry>\n"
			 "<entry>" entries "</entry>\n"
			 "<entry><para>%s</para></entry>\n" 
			 "</row>\n")))
       (if (re-search-backward (concat "<row>\n"
				      "<entry>" version "</entry>\n"
				      "<entry>\\([^<]*\\)</entry>\n"
				      "<entry>\\([0-9]*\\)</entry>\n"
				      "<entry>\\([0-9]*\\)</entry>\n"
				      "<entry>\\([0-9]*\\)</entry>\n"
				      "<entry>\\([0-9]*\\)</entry>\n"
				      "<entry><para>\\([^<]*\\)</para></entry>\n" 
				      "</row>\n") nil t)
	   (replace-match (format sizes (match-string 6)))
	   (insert (concat "\n" (format sizes ""))))
       (beginning-of-buffer)
       ;; replace the date near the beginning of the file
       (re-search-forward "<!ENTITY date\\(  *\\)'\\([^']*\\)'>")
       (replace-match date t nil nil 2))))

(defun patch-sizes ()
  "Patch the master twice so the size numbers will be up to date."
  (interactive)
  (message "First size check...")
  (basic-patch-sizes)
  (message "Patching...")
  (basic-save-buffer)
  (message "Second size check...")
  (basic-patch-sizes)
  (message "Patching...")
  (basic-save-buffer)
  (message "Done.")
  )

(provide 'jargon)

;;; jargon-mode.el ends here
