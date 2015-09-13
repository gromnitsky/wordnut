:; exec emacs -Q --script "$0" -- "$@" # -*- lexical-binding: t -*-

(setq tdd-lib-dir (concat (file-name-directory load-file-name) "/.."))
(push tdd-lib-dir load-path)
(push (file-name-directory load-file-name) load-path)

(require 'wordnut)
(require 'tdd-helper)

;; shut up reporters
(defun make-progress-reporter (message &optional min-value max-value
				       current-value min-change min-time))
(defun progress-reporter-update (reporter &optional value))
(defun progress-reporter-done (reporter))

(defun pp-hist ()
  (message "back:%s forw:%s cur:%s" wordnut-hist-back wordnut-hist-forw wordnut-hist-cur))



(ert-deftest history-back-forw()
  (wordnut-history-clean)

  (wordnut-lookup "1")
  (should (equal
	   (nth 1 (should-error (wordnut-history-backward)))
	   "No more backward history"))

  (should (equal wordnut-hist-cur "1"))
  (should (equal wordnut-hist-back nil) )
  (should (equal wordnut-hist-forw '("1")) )

  (should (equal
	   (nth 1 (should-error (wordnut-history-backward)))
	   "No items in the back history"))

  (wordnut-lookup "2")
  (wordnut-lookup "3")
  (wordnut-history-forward)

  (should (equal wordnut-hist-cur "1"))
  (should (equal wordnut-hist-back '("1" "3" "2")) )
  (should (equal wordnut-hist-forw nil) )

  (wordnut-history-backward)

  (should (equal wordnut-hist-cur "3"))
  (should (equal wordnut-hist-back '("3" "2")) )
  (should (equal wordnut-hist-forw '("1")) )

  (wordnut-history-backward)

  (should (equal wordnut-hist-cur "2"))

  (should (equal
	   (nth 1 (should-error (wordnut-history-backward)))
	   "No more backward history"))

;  (pp-hist)
  )

(ert-deftest history-dups()
  (wordnut-history-clean)

  (wordnut-lookup "1")
  (wordnut-lookup "1")

  (should (equal wordnut-hist-back '("1")) )

  (wordnut-lookup "2")
  (wordnut-lookup "3")
  (wordnut-lookup "1")

  (should (equal wordnut-hist-back '("1" "3" "2")) )
  )

(ert-deftest history-max()
  (wordnut-history-clean)
  (let ((oldmax wordnut-hist-max))
    (setq wordnut-hist-max 4)

    (wordnut-lookup "1")
    (wordnut-lookup "2")
    (wordnut-lookup "3")
    (wordnut-lookup "4")
    (wordnut-lookup "5")
    (wordnut-lookup "6")

    (should (equal wordnut-hist-back '("6" "5" "4" "3")) )
    (setq wordnut-hist-max oldmax)
    ))



(ert-run-tests-batch-and-exit (car argv))
