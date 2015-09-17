;; wordnut.el -- Major mode interface to WordNet -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'outline)
(require 'imenu)

(defconst wordnut-meta-name "wordnut")
(defconst wordnut-meta-version "0.0.1")

(defconst wordnut-bufname "*WordNut*")
(defconst wordnut-cmd "wn")
(defconst wordnut-cmd-options
  '("-over"
    "-synsn" "-synsv" "-synsa" "-synsr"
    "-simsv"
    "-antsn" "-antsv" "-antsa" "-antsr"
    "-famln" "-famlv" "-famla" "-famlr"
    "-hypen" "-hypev"
    "-hypon" "-hypov"
    "-treen" "-treev"
    "-coorn" "-coorv"
    "-derin" "-deriv"
    "-domnn" "-domnv" "-domna" "-domnr"
    "-domtn" "-domtv" "-domta" "-domtr"
    "-subsn"
    "-partn"
    "-membn"
    "-meron"
    "-hmern"
    "-sprtn"
    "-smemn"
    "-ssubn"
    "-holon"
    "-hholn"
    "-entav"
    "-framv"
    "-causv"
    "-perta" "-pertr"
    "-attrn" "-attra"))

(defconst wordnut-section-headings
  '("Antonyms" "Synonyms" "Hyponyms" "Troponyms"
    "Meronyms" "Holonyms" "Pertainyms"
    "Member" "Substance" "Part"
    "Attributes" "Derived" "Domain" "Familiarity"
    "Coordinate" "Grep" "Similarity"
    "Entailment" "'Cause To'" "Sample" "Overview of"))

(defconst wordnut-hist-max 20)
(defvar wordnut-hist-back '())
(defvar wordnut-hist-forw '())
(defvar wordnut-hist-cur nil)
(defvar wordnut-completion-hist '())



(define-derived-mode wordnut-mode outline-mode "WordNut"
  "Major mode interface to WordNet lexical database.
Turning on wordnut mode runs the normal hook `wordnut-mode-hook'.

\\{wordnut-mode-map}"

  (setq buffer-read-only t)
  (setq truncate-lines nil)
  (setq-local visual-line-fringe-indicators '(nil top-right-angle))
  (visual-line-mode 1)

  ;; we make a custom imenu index
  (setq imenu-generic-expression nil)
  (setq-local imenu-create-index-function 'wordnut--imenu-make-index)
  (imenu-add-menubar-index)

  ;; if user has adaptive-wrap mode installed, use it
  (if (fboundp 'adaptive-wrap-prefix-mode)
      (progn
	(setq adaptive-wrap-extra-indent 3)
	(adaptive-wrap-prefix-mode 1))))

(define-key wordnut-mode-map (kbd "q") 'delete-window)
(define-key wordnut-mode-map (kbd "RET") 'wordnut-lookup-current-word)
(define-key wordnut-mode-map (kbd "l") 'wordnut-history-backward)
(define-key wordnut-mode-map (kbd "r") 'wordnut-history-forward)
(define-key wordnut-mode-map (kbd "h") 'wordnut-lookup-history)
(define-key wordnut-mode-map (kbd "/") 'wordnut-search)
(define-key wordnut-mode-map (kbd "o") 'wordnut-show-overview)

(define-key wordnut-mode-map [(meta down)] 'outline-next-visible-heading)
(define-key wordnut-mode-map [(meta up)] 'outline-previous-visible-heading)
(define-key wordnut-mode-map (kbd "TAB") 'outline-toggle-children)

(define-key wordnut-mode-map (kbd "b") 'scroll-down-command)
(define-key wordnut-mode-map (kbd "DEL") 'scroll-down-command)
(define-key wordnut-mode-map (kbd "SPC") 'scroll-up-command)

;; this mode is suitable only for specially formatted data
(put 'wordnut-mode 'mode-class 'special)

(defun wordnut--completing (input)
  (let ((completion-ignore-case t))
    (completing-read "WordNut: "
		     (completion-table-dynamic 'wordnut--suggestions)
		     nil nil input 'wordnut-completion-hist)))

(defun wordnut--suggestions (input)
  (if (not (string-match "^\s*$" input))
      (progn
	(setq input (string-trim input))
	(let ((result (wordnut--exec input "-grepn" "-grepv" "-grepa" "-grepr")))
	  (if (equal "" result)
	      nil				; no match
	    (setq result (split-string result "\n"))
	    (wordnut--filter (lambda (idx)
			       (and
				(not (string-prefix-p "Grep of " idx))
				(not (equal idx ""))))
			     result))
	  ))))

(defun wordnut--exec (word &rest args)
  "Like `system(3)' but only for wn(1)."
  (with-output-to-string
    (with-current-buffer
	standard-output
      (apply 'call-process wordnut-cmd nil t nil word args)
      )))

(defun wordnut-search (word)
  "Prompt for a word to search for, then do the lookup."
  (interactive (list (wordnut--completing (current-word t t))))
  (wordnut--lookup word))

(defun wordnut--fix-name (str)
  (let ((max 10))
    (if (> (length str) max)
	(concat (substring str 0 max) "...")
      str)
    ))

(defun wordnut--lookup (word &optional dont-modify-history)
  "If wn prints something to stdout it means the word is
found. Otherwise we run wn again but with its -grepX options. If
that returns nothing or a list of words, prompt for a word, then
rerun `wordnut--lookup' with the selected word."
  (if (or (null word) (string-match "^\s*$" word)) (user-error "Invalid query"))

  (setq word (string-trim word))
  (let ((progress-reporter
	 (make-progress-reporter
	  (format "WordNet lookup for `%s'... " (wordnut--fix-name word)) 0 2))
	result buf)

    (setq result (apply 'wordnut--exec word wordnut-cmd-options))
    (progress-reporter-update progress-reporter 1)

    (if (equal "" result)
	(let (sugg)
	  (setq sugg (wordnut--suggestions word))
	  (setq word (if (listp sugg) (wordnut--completing word) sugg))
	  ;; recursion!
	  (wordnut--lookup word dont-modify-history))
      ;; else
      (if (not dont-modify-history)
	  (setq wordnut-hist-back (wordnut--hist-add word wordnut-hist-back)))
      (setq wordnut-hist-cur word)

      (setq buf (get-buffer-create wordnut-bufname))
      (with-current-buffer buf
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (insert result))
	(wordnut--format-buffer)
	(setq imenu--index-alist nil)	; flush imenu cache
	(set-buffer-modified-p nil)
	(unless (eq major-mode 'wordnut-mode) (wordnut-mode))
	(show-all)
	(wordnut--headerline))

      (progress-reporter-update progress-reporter 2)
      (progress-reporter-done progress-reporter)
      (wordnut--switch-to-buffer buf))
    ))

(defun wordnut-lookup-current-word ()
  (interactive)
  (wordnut--lookup (current-word)))

(defun wordnut--switch-to-buffer (buf)
  (unless (eq (current-buffer) buf)
    (unless (cdr (window-list))
      (split-window-vertically))
    (other-window 1)
    (switch-to-buffer buf)))

(defun wordnut--headerline ()
  (let (get-hist-item get-len)
    (setq get-hist-item (lambda (list)
			  (or (if (equal (car list) wordnut-hist-cur)
				  (nth 1 list) (car list)) "∅")))
    (setq get-len (lambda (list)
		    (if (equal (car list) wordnut-hist-cur)
			(1- (length list))
		      (length list))))

    (setq header-line-format
	  (format "C: %s, ← %s (%d), → %s (%d)"
		  (propertize (wordnut--fix-name wordnut-hist-cur) 'face 'bold)
		  (wordnut--fix-name (funcall get-hist-item wordnut-hist-back))
		  (funcall get-len wordnut-hist-back)
		  (wordnut--fix-name (funcall get-hist-item wordnut-hist-forw))
		  (funcall get-len wordnut-hist-forw)
		  )
	  )))

(defun wordnut--hist-slice (list)
  (remove nil (cl-subseq list 0 wordnut-hist-max)))

(defun wordnut--hist-add (val list)
  "Return a new list."
  (wordnut--hist-slice (if (member val list)
			  (cons val (remove val list))
			(cons val list)
			)))

(defun wordnut-history-clean ()
  (interactive)
  (setq wordnut-hist-back '())
  (setq wordnut-hist-forw '())
  (setq wordnut-hist-cur nil)
  )

(defun wordnut-lookup-history ()
  (interactive)
  (let ((items (append wordnut-hist-back wordnut-hist-forw)))
    (unless items (user-error "History is empty"))
    (wordnut--lookup (ido-completing-read "wordnut history: " items) t)
    ))

(defmacro wordnut-hist--extract (desc from to)
  `(let (word)
     (unless ,from (user-error "The %s history is ∅" ,desc))

     ;; move the item from FROM to TO
     (setq word (pop ,from))
     (setq ,to (wordnut--hist-add word ,to))

     (if (equal word wordnut-hist-cur) (setq word (car ,from)))
     (unless word (user-error "No more %s history" ,desc))
     (wordnut--lookup word t)))

(defun wordnut-history-backward ()
  (interactive)
  (wordnut-hist--extract "backward" wordnut-hist-back wordnut-hist-forw))

(defun wordnut-history-forward ()
  (interactive)
  (wordnut-hist--extract "forward" wordnut-hist-forw wordnut-hist-back))

(defun wordnut--imenu-make-index ()
  (let ((index '()) marker)
    (save-excursion
      (while (re-search-forward "^\\* \\(.+\\)$" nil t)
	(setq marker (make-marker))
	(set-marker marker (line-beginning-position))
	(push `(,(match-string 1) . ,marker) index))

      (reverse index))))

(defun wordnut--format-buffer ()
  (let ((inhibit-read-only t)
	(case-fold-search nil))
    ;; delete the 1st empty line
    (goto-char (point-min))
    (delete-char 1)

    ;; make headings
    (delete-matching-lines "^ +$" (point-min) (point-max))
    (while (re-search-forward
	    (concat "^" (regexp-opt wordnut-section-headings t)) nil t)
      (replace-match "* \\1"))

    ;; remove empty entries
    (goto-char (point-min))
    (while (re-search-forward "^\\* .+\n\n\\*" nil t)
      (replace-match "*" t t)
      ;; back over the '*' to remove next matching lines
      (backward-char))

    ;; make sections
    (goto-char (point-min))
    (while (re-search-forward "^Sense [0-9]+" nil t)
      (replace-match "** \\&"))

    ;; remove the last empty entry
    (goto-char (point-max))
    (if (re-search-backward "^\\* .+\n\\'" nil t)
	(replace-match "" t t))

    (goto-char (point-min))
    ))



(defun wordnut--lexi-cat ()
  "Return a category name for the current lexical category."
  (let (line match)
    (save-excursion
      (ignore-errors
	(outline-up-heading 1))
      (setq line (substring-no-properties (thing-at-point 'line)))
      (unless (string-match " of \\(noun\\|verb\\|adj\\|adv\\)" line)
	(user-error "Cannot extract a lexical category"))

      (match-string 1 line)
      )))

(defun wordnut--lexi-sense ()
  "Return a sense number for the current lexical category."
  (let (line match)
    (save-excursion
      (ignore-errors
	(outline-up-heading -1))
      (setq line (substring-no-properties (thing-at-point 'line)))
      (unless (string-match "Sense \\([0-9]+\\)" line)
	(user-error "Cannot extract a sense number; move the cursor to the proper place first"))

      (match-string 1 line)
      )))

(defun wordnut--lexi-info-inline ()
  "Return a list '(word cat sense) from the current line or nil."
  (let ((line (substring-no-properties (thing-at-point 'line))))
    (if (string-match "->\\((.+)\\)? \\(.+\\)#\\([0-9]+\\)" line)
	(list (match-string 2 line)
	      (if (match-string 1 line)
		  (replace-regexp-in-string "[()]" "" (match-string 1 line)))
	      (match-string 3 line))
      nil)))

(defun wordnut--lexi-overview ()
  "Try to locale an 'Overview' heading to extract a 'sense'
of a current lexical category.

Return a list '(cat sense desc)."
  (let (desc cat sense inline)
    (save-excursion
      (setq inline (wordnut--lexi-info-inline))
      (if inline
	  (if (equal (car inline) (wordnut--lexi-word))
	      (progn
		(setq cat (nth 1 inline))
		(setq sense (nth 2 inline)))
	    ;; FIXME
	    (user-error "Inline reference extraction is not implemented")))

      (setq cat (or cat (wordnut--lexi-cat)))
      (setq sense (or sense (wordnut--lexi-sense)))

      (goto-char (point-min))
      (re-search-forward (format "^\\* Overview of %s" cat) nil t)
      (next-line)
      (re-search-forward (format "%s\\. " sense) nil t)
      (setq desc (string-trim
		  (substring-no-properties (thing-at-point 'line))))

      (unless desc (user-error "Failed to extract an overview"))
      (list cat sense desc)
      )))

(defun wordnut--lexi-word ()
  "Return an actual displayed word, not what a user has typed
for a query. For example, return 'do' instead of 'did'."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Overview of [^ ]+ \\(.+\\)$" nil t)
      (user-error "Cannot extract the actual current word"))
    (substring-no-properties
     (replace-regexp-in-string "_" " " (match-string 1)))))

(defun wordnut-show-overview ()
  "Show a tooltip of a 'sense' for the current lexical category."
  (interactive)
  (let ((buf (get-buffer wordnut-bufname)) desc)
    (unless buf (user-error "Has %s buffer been killed?" wordnut-bufname))

    (with-current-buffer buf
      (setq desc (wordnut--lexi-overview))
      (tooltip-show (wordnut--word-wrap
		     (+ (/ (window-body-width) 2) (/ (window-body-width) 4))
		     (format "OVERVIEW `%s', %s\n\n%s"
			     (wordnut--fix-name (wordnut--lexi-word))
			     (car desc) (nth 2 desc)
			     )))
      )))



;; s.el
(defun wordnut--word-wrap (len s)
  "If S is longer than LEN, wrap the words with newlines."
  (with-temp-buffer
    (insert s)
    (let ((fill-column len))
      (fill-region (point-min) (point-max)))
    (buffer-substring-no-properties (point-min) (point-max))))

;; emacswiki.org
(defun wordnut--filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(provide 'wordnut)
