:; exec emacs -Q --script "$0" -- "$@" # -*- lexical-binding: t -*-

(setq tdd-lib-dir (concat (file-name-directory load-file-name) "/.."))
(push tdd-lib-dir load-path)
(push (file-name-directory load-file-name) load-path)

(require 'wn-org2)
(require 'tdd-helper)

;; shut up reporters
(defun make-progress-reporter (message &optional min-value max-value
				       current-value min-change min-time))
(defun progress-reporter-update (reporter &optional value))
(defun progress-reporter-done (reporter))

(defun pp-hist ()
  (message "back:%s forw:%s cur:%s" wn-org2-hist-back wn-org2-hist-forw wn-org2-hist-cur))



(ert-deftest history-back-forw()
  (wn-org2-history-clean)

  (wn-org2-lookup "1")
  (should (equal
	   (nth 1 (should-error (wn-org2-history-backward)))
	   "No more backward history"))

  (should (equal wn-org2-hist-cur "1"))
  (should (equal wn-org2-hist-back nil) )
  (should (equal wn-org2-hist-forw '("1")) )

  (should (equal
	   (nth 1 (should-error (wn-org2-history-backward)))
	   "No items in the back history"))

  (wn-org2-lookup "2")
  (wn-org2-lookup "3")
  (wn-org2-history-forward)

  (should (equal wn-org2-hist-cur "1"))
  (should (equal wn-org2-hist-back '("1" "3" "2")) )
  (should (equal wn-org2-hist-forw nil) )

  (wn-org2-history-backward)

  (should (equal wn-org2-hist-cur "3"))
  (should (equal wn-org2-hist-back '("3" "2")) )
  (should (equal wn-org2-hist-forw '("1")) )

  (wn-org2-history-backward)

  (should (equal wn-org2-hist-cur "2"))

  (should (equal
	   (nth 1 (should-error (wn-org2-history-backward)))
	   "No more backward history"))

;  (pp-hist)
  )

(ert-deftest history-dups()
  (wn-org2-history-clean)

  (wn-org2-lookup "1")
  (wn-org2-lookup "1")

  (should (equal wn-org2-hist-back '("1")) )

  (wn-org2-lookup "2")
  (wn-org2-lookup "3")
  (wn-org2-lookup "1")

  (should (equal wn-org2-hist-back '("1" "3" "2")) )
  )

(ert-deftest history-max()
  (wn-org2-history-clean)
  (let ((oldmax wn-org2-hist-max))
    (setq wn-org2-hist-max 4)

    (wn-org2-lookup "1")
    (wn-org2-lookup "2")
    (wn-org2-lookup "3")
    (wn-org2-lookup "4")
    (wn-org2-lookup "5")
    (wn-org2-lookup "6")

    (should (equal wn-org2-hist-back '("6" "5" "4" "3")) )
    (setq wn-org2-hist-max oldmax)
    ))



(ert-run-tests-batch-and-exit (car argv))
