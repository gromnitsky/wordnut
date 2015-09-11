;; wn-org2.el -- an org-mode interface for WordNet

(defconst wn-org2-meta-name "wm-org2")
(defconst wn-org2-meta-version "0.0.1")

(defconst wn-org2-bufname "*WordNet*")

(defgroup wn-org2 nil
  "WordNet org-mode interface."
  :group 'wn-org2)

(defcustom wn-org2-command "wn"
  "Shell command for WordNet wn(1)."
  :type 'string
  :group 'wn-org2)

(defcustom wn-org2-mode-hook nil
  "Normal hook run after entering wn-org2 mode."
  :type 'hook
  :group 'wn-org2)

(defcustom wn-org2-options
  '("-over"
    "-antsn" "-antsv" "-antsa" "-antsr"
    "-hypen" "-hypev"
    "-hypon" "-hypov"
    "-entav"
    "-synsn" "-synsv" "-synsa" "-synsr"
    "-smemn"
    "-ssubn"
    "-sprtn"
    "-membn"
    "-subsn"
    "-partn"
    "-meron"
    "-holon"
    "-causv"
    "-perta" "-pertr"
    "-attrn" "-attra"
    "-derin" "-deriv"
    "-domnn" "-domnv" "-domna" "-domnr"
    "-domtn" "-domtv" "-domta" "-domtr"
    "-famln" "-famlv" "-famla" "-famlr"
    "-framv"
    "-coorn" "-coorv"
    "-simsv"
    "-hmern"
    "-hholn")
"Command line options for wn"
  :type 'list
  :group 'wn-org2)

(defcustom wn-org2-section-headings
  '("Antonyms" "Synonyms" "Hyponyms" "Troponyms"
    "Meronyms" "Holonyms" "Pertainyms"
    "Member" "Substance" "Part"
    "Attributes" "Derived" "Domain" "Familiarity"
    "Coordinate" "Grep" "Similarity"
    "Entailment" "'Cause To'" "Sample" "Overview")
  "Section headings to which the `*' org-mode section label is added."
  :type 'list
  :group 'wn-org2)



(define-derived-mode wn-org2-mode
  org-mode "WordNet"
  "Major mode for WordNet dictionary search.

\\{wn-org2-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil)
  (run-hooks 'wn-org2-mode-hook))

(define-key wn-org2-mode-map (kbd "q") 'wn-org2-quit)

(defun wn-org2-quit ()
  "Kill WordNet windows and buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (delete-windows-on buffer)
    (kill-buffer buffer)))

(defun wn-org2-replace-regexp (regexp-string repl-string)
  (goto-char (point-min))
  (while (re-search-forward regexp-string nil t)
    (replace-match repl-string t nil)))

;; emacswiki.org
(defun wn-org2-filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;; emacswiki.org
(defun wn-org2-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
				    (: (* (any " \t\n")) eos)))
			    ""
			    str))

(defun wn-org2-suggest (word)
  "ido suggestions"
  (if (string-match "^\s*$" word) (error "a non-empty string arg required"))
  (setq word (wn-org2-chomp word))

  (let ((result (wn-org2-exec word "-grepn" "-grepv" "-grepa" "-grepr"))
	(suggestions nil))
    (if (string= "" result) (error "Refine your query"))

    (setq result (split-string result "\n"))
    (setq suggestions (wn-org2-filter (lambda (idx)
				       (and
					(not (string-prefix-p "Grep of " idx))
					(not (string= idx ""))))
				     result))
    (ido-completing-read "WordNet: " suggestions)
    ))

(defun wn-org2-exec (word &rest args)
  "Like `system(3)' but only for wordnet"
  (with-output-to-string
    (with-current-buffer
	standard-output
      (apply 'call-process wn-org2-command nil t nil word args)
      )))

;; If wm prints something to stdout it means the word is
;; found. Otherwise we run wn again but with its -grepX options. If
;; that returns nothing, bail out. If we get a list of words, show
;; them to the user, then rerun `wn-org2-search' with the selected
;; word.
(defun wn-org2-search (word)
  "Search WordNet for WORD if provided otherwise prompt for it.
The word at the point is suggested which can be replaced."
  (interactive (list (read-string "WordNet: " (current-word))))
  (if (string-match "^\s*$" word) (error "Invalid query"))

  (setq word (wn-org2-chomp word))
  (let ((result (apply 'wn-org2-exec word wn-org2-options)))
    (if (string= "" result)
	;; recursion!
	(wn-org2-search (wn-org2-suggest word))
      (progn
	(let ((buf (get-buffer-create wn-org2-bufname)))
	  (with-current-buffer buf
	    (let ((inhibit-read-only t))
	      (erase-buffer)
	      (insert result)

	      (delete-matching-lines "^ *$" (point-min) (point-max))
	      (wn-org2-replace-regexp
	       (concat "^" (regexp-opt wn-org2-section-headings t)) "* \\1")

	      ;; Remove empty entries, those followed by a '*' on the next line
	      (goto-char (point-min))
	      (while (re-search-forward "^\\*.*\n\\*" nil t)
		(replace-match "*" t t)
		;; Back over the '*' to remove following empty entries
		(backward-char))

	      (wn-org2-replace-regexp "^Sense \\([1-9]*\\)" "  \\1. ")
	      (wn-org2-replace-regexp "       [=*]>" "    +")
	      (unless (eq major-mode 'wn-org2-mode)
		(wn-org2-mode))
	      (indent-region (point-min) (point-max))
	      (fill-region (point-min) (point-max))
	      (goto-char (point-min))
	      ))
	  (wn-org2-switch-to-buffer buf)
	  ))
      )))

(defun wn-org2-switch-to-buffer (buf)
  (unless (eq (current-buffer) buf)
    (unless (cdr (window-list))
      (split-window-vertically))
    (other-window 1)
    (switch-to-buffer buf)))

(provide 'wn-org2)
