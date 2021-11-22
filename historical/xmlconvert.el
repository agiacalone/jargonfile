;; Scrap code we might use gain someday.

(defun jargon-get-glossdev ()
  "Get the current alphanumeric key."
  (save-excursion
    (if (re-search-backward "<glossdiv><title>\\([A-Z]*\\)</title>" nil t)
	(match-string 1)
      "unlettered")))

(defun get-steele ()
  (interactive)  
  (let ((outbuf (set-buffer (get-buffer-create "*out*"))))
    (erase-buffer)
    (set-buffer (get-buffer-create "*plugh*"))
    (erase-buffer)
    (insert-file "jargon.xml")
    (insert-file "chaff.xml")
    (beginning-of-buffer)
    (while t
      (jargon-next-entry)
      (narrow-to-region (point)
			(save-excursion (jargon-end-of-entry)))
      (if (search-forward "Steele-1983" nil t)
	  (progn
	    (goto-char (point-min))
	    (print (jargon-this-headword) outbuf)))
      (widen))))

;;
;;  Process output of diffreport -l
;;
;; These functions have to be called with insertions going to an empty
;; and writeable scratch buffer.  The variable known-missing needs to
;; be defined as a list if missing release levels in dotted format.

(defun jargon-diff-message (&rest fields)
  (save-excursion
    (set-buffer "*grind*")
    (insert (apply 'format fields) "\n")))

(defun jargon-diff-goto (headword)
  (setq headword (jargon-xmlize headword))
  (jargon-entry-file headword)
  (cond ((jargon-goto-entry headword)
	 t)
	((progn 
	   ;; Can't use binary search, chaff entries are not alpha-sorted
	   (find-file "chaff.xml") 
	   (goto-char (point-min))
	   (prog1
	       (let ((case-fold-search t))
		 (re-search-forward (concat 
				     "^<glossentry><glossterm>" 
				     (regexp-quote headword) 
				     "</glossterm>") nil t))
	     (beginning-of-line)))
	 t)
	(t 
	 (jargon-diff-message "diff: could not find '%s' anywhere." headword)
	 nil)))

(defun jargon-diff-add (rev headword)
  "Create a revhistory entry noting when the headword was added."
  (and
   (jargon-diff-goto headword)
   (let ((oldtag (jargon-get-revremark rev)))
     (cond ((not oldtag) 
	    (jargon-set-revremark "Added: (deduced from diffs)" rev))
	   ((string-match "^Changed" oldtag)
	    nil)
	   ((not (string-match "^Added" oldtag))
	    (jargon-diff-message "diff: addition conflict with '%s' at %s:%s" oldtag headword rev))))))

(defun jargon-diff-delete (rev headword)
  "Create a revhistory entry noting when the headword was deleted."
  (and 
   (jargon-diff-goto headword)
   (let ((oldtag (jargon-get-revremark rev)))
     (cond ((not oldtag) 
	    (jargon-set-revremark "Deleted: (deduced from diffs)" rev))
	   ((not (string-match "^Deleted" oldtag))
	    (jargon-diff-message "diff: deletion conflict with '%s' at %s:%s" oldtag headword rev))))))

(defun jargon-diff-rename (rev headword old new)
  "Create a revhistory entry noting when the headword was renamed."
    (and 
     (jargon-diff-goto headword)
     (let ((oldtag (jargon-get-revremark rev))
	   (newtag (format "Renamed: %s -> %s (deduced from diffs)" old new)))
       (cond ((not oldtag) 
	      (jargon-set-revremark newtag rev))
	     ((not (string= oldtag newtag))
	      (jargon-diff-message "diff: rename conflict with '%s' at %s:%s" oldtag headword rev))))))

(defun jargon-diff-revert ()
  "Undo the results of a diff interpretatuion."
  (interactive)
  (set-buffer "jargon.xml")
  (revert-buffer t t)
  (set-buffer "chaff.xml")
  (revert-buffer t t)
  )
 
