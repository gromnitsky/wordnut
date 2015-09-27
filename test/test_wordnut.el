:; exec emacs -Q --script "$0" -- "$@" # -*- lexical-binding: t -*-

(setq tdd-lib-dir (concat (file-name-directory load-file-name) "/.."))
(push tdd-lib-dir load-path)
(push (file-name-directory load-file-name) load-path)

(require 'tdd-helper)
(require 'wordnut)

(ert-deftest link-raw-empty-buf()
  (with-temp-buffer
    (should-not (wordnut--lexi-link-word-sense))
    ))

(ert-deftest link-raw-no-link()
  (with-temp-buffer
    (insert "foo")
    (goto-char (point-min))
    (should-not (wordnut--lexi-link-word-sense))
    ))

(ert-deftest link-raw-no-category-1()
  (with-temp-buffer
    (insert "Phrasal Verb-> part with#1, foo bar#22")
    (goto-char (point-min))
    (should (equal "part with#1" (wordnut--lexi-link-word-sense)))
    ))

(ert-deftest link-raw-no-category-2()
  (with-temp-buffer
    (insert "Phrasal Verb-> part with#1, foo bar#22")
    (goto-char (point-min))
    (re-search-forward ",")
    (should (equal "foo bar#22" (wordnut--lexi-link-word-sense)))
    ))

(ert-deftest link-raw-no-category-3()
  (with-temp-buffer
    (insert "Phrasal Verb-> part with#1; foo bar#22")
    (goto-char (point-min))
    (re-search-forward ";")
    (should (equal "foo bar#22" (wordnut--lexi-link-word-sense)))
    ))

(ert-deftest link-raw-no-category-4()
  (with-temp-buffer
    (insert "Phrasal Verb-> part with#1, foo bar#22")
    (should (equal "foo bar#22" (wordnut--lexi-link-word-sense)))
    ))

(ert-deftest link-raw-normal()
  (with-temp-buffer
    (insert "Phrasal Verb->(noun) part with#1, foo bar#22")
    (goto-char (point-min))
    (should (equal "part with#1" (wordnut--lexi-link-word-sense)))
    ))

(ert-deftest link-raw-broken()
  (with-temp-buffer
    (insert "Phrasal Verb->(noun) part with#1, foo bar")
    (goto-char (point-min))
    (re-search-forward ",")
    (should-not (wordnut--lexi-link-word-sense))
    ))



(ert-deftest link-no-link()
  (with-temp-buffer
    (insert "foo")
    (goto-char (point-min))
    (should-not (wordnut--lexi-link))
    ))

(ert-deftest link-normal()
  (with-temp-buffer
    (insert "Phrasal Verb->(noun) part with#1, foo bar#22")
    (goto-char (point-min))
    (should (equal '("part with" "noun" "1") (wordnut--lexi-link)))
    ))

(ert-deftest link-no-category()
  (with-temp-buffer
    (insert "Phrasal Verb-> part with#1, foo bar#22")
    (goto-char (point-min))
    (should (equal '("part with" nil "1") (wordnut--lexi-link)))
    ))



(ert-deftest lexi-overview()
  (wordnut-search "did")
  (should (equal "do" (wordnut--lexi-word)))
  (should-error (wordnut--lexi-overview))
  (goto-char (point-max))
  (re-search-backward "Doctor of Sacred Theology")
  (should (equal '("noun" "3" "3. Doctor of Osteopathy, DO -- (doctor's degree in osteopathy)") (wordnut--lexi-overview)))
  )




(ert-run-tests-batch-and-exit (car argv))
