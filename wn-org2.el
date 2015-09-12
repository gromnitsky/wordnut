;; wn-org2.el -- Major mode interface to WordNet -*- lexical-binding: t -*-

(defconst wn-org2-meta-name "wm-org2")
(defconst wn-org2-meta-version "0.0.1")

(defconst wn-org2-bufname "*WordNet*")
(defvar wn-org2-cmd "wn")
(defvar wn-org2-cmd-options
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
    "-hholn"))

(defvar wn-org2-section-headings
  '("Antonyms" "Synonyms" "Hyponyms" "Troponyms"
    "Meronyms" "Holonyms" "Pertainyms"
    "Member" "Substance" "Part"
    "Attributes" "Derived" "Domain" "Familiarity"
    "Coordinate" "Grep" "Similarity"
    "Entailment" "'Cause To'" "Sample" "Overview"))



(define-derived-mode wn-org2-mode
  org-mode "WordNet"
  "Major mode interface to WordNet lexical database.
Turning on WordNet mode runs the normal hook `wn-org2-mode-hook'.

\\{wn-org2-mode-map}"

  (setq buffer-read-only t)
  (setq truncate-lines nil))

(define-key wn-org2-mode-map (kbd "q") 'delete-window)
(define-key wn-org2-mode-map (kbd "RET") 'wn-org2-lookup-current-word)
(define-key wn-org2-mode-map (kbd "/") 'wn-org2-search)

;; this mode is suitable only for specially formatted data
(put 'wn-org2-mode 'mode-class 'special)

(defun wn-org2-suggest (word)
  "ido suggestions"
  (if (string-match "^\s*$" word) (error "a non-empty string arg required"))
  (setq word (wn-org2-chomp word))

  (let ((result (wn-org2-exec word "-grepn" "-grepv" "-grepa" "-grepr"))
	suggestions)
    (if (equal "" result) (user-error "Refine your query"))

    (setq result (split-string result "\n"))
    (setq suggestions (wn-org2-filter (lambda (idx)
				       (and
					(not (string-prefix-p "Grep of " idx))
					(not (equal idx ""))))
				     result))
    (ido-completing-read "WordNet: " suggestions)
    ))

(defun wn-org2-exec (word &rest args)
  "Like `system(3)' but only for wn(1)."
  (with-output-to-string
    (with-current-buffer
	standard-output
      (apply 'call-process wn-org2-cmd nil t nil word args)
      )))

(defun wn-org2-search (word)
  "Search WordNet for WORD if provided otherwise prompt for it.
The word at the point is suggested which can be replaced."
  (interactive (list (read-string "WordNet: " (current-word))))
  (wn-org2-lookup word)
  )

;; If wm prints something to stdout it means the word is
;; found. Otherwise we run wn again but with its -grepX options. If
;; that returns nothing, bail out. If we get a list of words, show
;; them to the user, then rerun `wn-org2-lookup' with the selected
;; word.
(defun wn-org2-lookup (word)
  (if (or (null word) (string-match "^\s*$" word)) (user-error "Invalid query"))

  (setq word (wn-org2-chomp word))
  (let ((result (apply 'wn-org2-exec word wn-org2-cmd-options)))
    (if (equal "" result)
	;; recursion!
	(wn-org2-lookup (wn-org2-suggest word))
      ;; else
      (let ((buf (get-buffer-create wn-org2-bufname)))
	(with-current-buffer buf
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (insert result))
	  (wn-org2-format-buffer))
	(wn-org2-switch-to-buffer buf))
      ))
  )

(defun wn-org2-lookup-current-word ()
  (interactive)
  (wn-org2-lookup (current-word)))

(defun wn-org2-switch-to-buffer (buf)
  (unless (eq (current-buffer) buf)
    (unless (cdr (window-list))
      (split-window-vertically))
    (other-window 1)
    (switch-to-buffer buf)))

;; FIXME: it should operate on a string, not on a buffer content
(defun wn-org2-format-buffer ()
  (let ((inhibit-read-only t))
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

;; FIXME: remove after writing a proper `wn-org2-format'
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

(provide 'wn-org2)
