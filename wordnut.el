;; wordnut.el -- Major mode interface to WordNet -*- lexical-binding: t -*-

(require 'cl-lib)

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



(define-derived-mode wordnut-mode outline-mode "WordNet"
  "Major mode interface to WordNet lexical database.
Turning on wordnut mode runs the normal hook `wordnut-mode-hook'.

\\{wordnut-mode-map}"

  (setq buffer-read-only t)
  (setq truncate-lines nil))

(define-key wordnut-mode-map (kbd "q") 'delete-window)
(define-key wordnut-mode-map (kbd "RET") 'wordnut-lookup-current-word)
(define-key wordnut-mode-map (kbd "l") 'wordnut-history-backward)
(define-key wordnut-mode-map (kbd "r") 'wordnut-history-forward)
(define-key wordnut-mode-map (kbd "h") 'wordnut-lookup-history)
(define-key wordnut-mode-map (kbd "/") 'wordnut-search)

;; this mode is suitable only for specially formatted data
(put 'wordnut-mode 'mode-class 'special)

(defun wordnut-suggest (word)
  "ido suggestions"
  (if (string-match "^\s*$" word) (error "a non-empty string arg required"))
  (setq word (wordnut-chomp word))

  (let ((result (wordnut-exec word "-grepn" "-grepv" "-grepa" "-grepr"))
	suggestions)
    (if (equal "" result) (user-error "Refine your query"))

    (setq result (split-string result "\n"))
    (setq suggestions (wordnut-filter (lambda (idx)
					(and
					 (not (string-prefix-p "Grep of " idx))
					 (not (equal idx ""))))
				      result))
    (ido-completing-read "WordNet: " suggestions)
    ))

(defun wordnut-exec (word &rest args)
  "Like `system(3)' but only for wn(1)."
  (with-output-to-string
    (with-current-buffer
	standard-output
      (apply 'call-process wordnut-cmd nil t nil word args)
      )))

(defun wordnut-search (word)
  "Prompt for a word to search for, then do the lookup."
  (interactive (list (read-string "WordNet: " (current-word))))
  (wordnut-lookup word))

(defun wordnut-fix-name (str)
  (let ((max 10))
    (if (> (length str) max)
	(concat (substring str 0 max) "...")
      str)
    ))

;; If wn prints something to stdout it means the word is
;; found. Otherwise we run wn again but with its -grepX options. If
;; that returns nothing, bail out. If we get a list of words, show
;; them to the user, then rerun `wordnut-lookup' with the selected
;; word.
(defun wordnut-lookup (word &optional dont-modify-history)
  (if (or (null word) (string-match "^\s*$" word)) (user-error "Invalid query"))

  (setq word (wordnut-chomp word))
  (let ((progress-reporter
	 (make-progress-reporter
	  (format "WordNet lookup for `%s'... " (wordnut-fix-name word)) 0 2))
	result buf)

    (setq result (apply 'wordnut-exec word wordnut-cmd-options))
    (progress-reporter-update progress-reporter 1)

    (if (equal "" result)
	;; recursion!
	(wordnut-lookup (wordnut-suggest word))
      ;; else
      (if (not dont-modify-history)
	  (setq wordnut-hist-back (wordnut-hist-add word wordnut-hist-back)))
      (setq wordnut-hist-cur word)

      (setq buf (get-buffer-create wordnut-bufname))
      (with-current-buffer buf
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (insert result))
	(wordnut-format-buffer)
	(unless (eq major-mode 'wordnut-mode) (wordnut-mode))
	(show-all)
	(wordnut-headerline))

      (progress-reporter-update progress-reporter 2)
      (progress-reporter-done progress-reporter)
      (wordnut-switch-to-buffer buf))
    ))

(defun wordnut-lookup-current-word ()
  (interactive)
  (wordnut-lookup (current-word)))

(defun wordnut-switch-to-buffer (buf)
  (unless (eq (current-buffer) buf)
    (unless (cdr (window-list))
      (split-window-vertically))
    (other-window 1)
    (switch-to-buffer buf)))

(defun wordnut-headerline ()
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
		  (wordnut-fix-name wordnut-hist-cur)
		  (wordnut-fix-name (funcall get-hist-item wordnut-hist-back))
		  (funcall get-len wordnut-hist-back)
		  (wordnut-fix-name (funcall get-hist-item wordnut-hist-forw))
		  (funcall get-len wordnut-hist-forw)
		  )
	  )))

(defun wordnut-hist-slice (list)
  (remove nil (cl-subseq list 0 wordnut-hist-max)))

(defun wordnut-hist-add (val list)
  "Return a new list."
  (wordnut-hist-slice (if (member val list)
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
    (wordnut-lookup (ido-completing-read "wordnut history: " items) t)
    ))

(defun wordnut-history-backward ()
  (interactive)
  (unless wordnut-hist-back (user-error "No items in the back history"))

  (let ((word (pop wordnut-hist-back)))
    (setq wordnut-hist-forw (wordnut-hist-add word wordnut-hist-forw))
    (if (equal word wordnut-hist-cur) (setq word (car wordnut-hist-back)))
    (if (not word) (user-error "No more backward history"))
    (wordnut-lookup word t)))

;; meet the evil twin!
(defun wordnut-history-forward ()
  (interactive)
  (unless wordnut-hist-forw (user-error "No items in the forward history"))

  (let ((word (pop wordnut-hist-forw)))
    (setq wordnut-hist-back (wordnut-hist-add word wordnut-hist-back))
    (if (equal word wordnut-hist-cur) (setq word (car wordnut-hist-forw)))
    (if (not word) (user-error "No more forward history"))
    (wordnut-lookup word t)))

;; FIXME: it should operate on a string, not on a buffer content
(defun wordnut-format-buffer ()
  (let ((inhibit-read-only t))
    ;; delete the 1st empty line
    (goto-char (point-min))
    (delete-char 1)

    ;; make headlines
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

    ;; remove the last empty entry
    (goto-char (point-max))
    (if (re-search-backward "^\\* .+\n\\'" nil t)
	(replace-match "" t t))

    (goto-char (point-min))
    ))



;; emacswiki.org
(defun wordnut-filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;; emacswiki.org
(defun wordnut-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
				    (: (* (any " \t\n")) eos)))
			    ""
			    str))

(provide 'wordnut)
